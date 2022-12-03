use 5.014;
use strict;
use warnings;

package Newtype;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.001';

use Type::Tiny::Class 2.000000;
use parent 'Type::Tiny::Class';

use Types::Common -types;
use namespace::autoclean;

sub _exporter_fail {
	my ( $class, $name, $opts, $globals ) = @_;
	my $caller = $globals->{into};

	$opts->{caller} = $caller;
	$opts->{name}   = $name;
	$opts->{class}  = sprintf '%s::Newtype::%s', $opts->{caller}, $opts->{name};
	my $type = $class->new( $opts );

	$INC{'Type/Registry.pm'}
		? 'Type::Registry'->for_class( $caller )->add_type( $type )
		: ( $Type::Registry::DELAYED{$caller}{$type->name} = $type )
		unless( ref($caller) or $caller eq '-lexical' or $globals->{'lexical'} );
	return map +( $_->{name} => $_->{code} ), @{ $type->exportables };
}

sub new {
	my $class = shift;
	my %opts = ( @_ == 1 and ref $_[0] ) ? %{ $_[0] } : @_;
	
	my $inner = $opts{inner}
		or die "Expected option: inner";
	if ( not ref $inner ) {
		$opts{inner} = $inner = 'Type::Tiny::Class'->new( class => $inner );
	}
	
	use B ();
	my $type = $class->SUPER::new( %opts );
	$type->_make_newclass;
	my $coercion = sprintf(
		q{do { my $x = $_; bless( \$x, %s ) }},
		B::perlstring( $opts{class} ),
	);
	$type->coercion->add_type_coercions( $inner, $coercion );
	return $type;
}

sub inner_type { $_[0]{inner} }

sub exportables {
	my $self = shift;
	my @orig = @{ $self->SUPER::exportables( @_ ) };
	my @drop = grep { $_->{tags}[0] eq 'types' } @orig;
	my @keep = grep { $_->{tags}[0] ne 'types' } @orig;

	return [
		$self->_newtype_exportables( @drop ),
		@keep,
	];
}

sub _newtype_exportables {
	my ( $self, $old ) = @_;
	my %old = %$old;
	$old{code} = sub (;$) {
		my ( $inner_value, @rest ) = @_
			or return $self;
		my $wrapped_value = bless( \$inner_value, $self->{class} );
		wantarray ? ( $wrapped_value, @rest ) : $wrapped_value;
	};
	return ( \%old );
}

sub _make_newclass {
	my ( $self ) = @_;

	my $inner = $self->inner_type;
	my $kind = 'Object';
	my $overload;
	if ( $self->{kind} ) {
		$kind = $self->{kind};
	}
	elsif ( $inner->is_a_type_of( ArrayRef ) ) {
		$kind = 'Array';
		$overload = '( q[@{}] => sub { ${+shift} }, bool => sub { !!1 }, fallback => 1 )';
	}
	elsif ( $inner->is_a_type_of( Bool ) ) {
		$kind = 'Bool';
		$overload = '( bool => sub { !!${+shift} }, fallback => 1 )';
	}
	elsif ( $inner->is_a_type_of( CodeRef ) ) {
		$kind = 'Code';
		$overload = '( q[&{}] => sub { ${+shift} }, bool => sub { !!1 }, fallback => 1 )';
	}
	elsif ( $inner->is_a_type_of( Int ) ) {
		$kind = 'Counter';
		$overload = '( q[0+] => sub { ${+shift} }, bool => sub { ${+shift} }, fallback => 1 )';
	}
	elsif ( $inner->is_a_type_of( HashRef ) ) {
		$kind = 'Hash';
		$overload = '( q[%{}] => sub { ${+shift} }, bool => sub { !!1 }, fallback => 1 )';
	}
	elsif ( $inner->is_a_type_of( Num ) ) {
		$kind = 'Number';
		$overload = '( q[0+] => sub { ${+shift} }, bool => sub { ${+shift} }, fallback => 1 )';
	}
	elsif ( $inner->is_a_type_of( Str ) ) {
		$kind = 'String';
		$overload = '( q[""] => sub { ${+shift} }, bool => sub { ${+shift} }, fallback => 1 )';
	}
	$self->{kind} = $kind;

	my $class = $self->class;
	my $perl_code = "package $class;\n";
	$perl_code .= "use overload $overload;\n" if defined $overload;
	$perl_code .= q{
		sub INNER {
			my $self = shift;
			$$self;
		}
	};
	if ( $kind eq 'Object' ) {
		$perl_code .= q{
			sub AUTOLOAD {
				my $self = shift;
				my ( $method ) = ( our $AUTOLOAD =~ /::(\w+)$/ );
				if ( $method eq 'DESTROY' ) {
					my $found = $$self->can( 'DESTROY' ) or return;
					return $$self->$found( @_ );
				}
				$$self->$method( @_ );
			}
			sub isa {
				my $self = shift;
				$$self->isa( @_ ) or
					$self->UNIVERSAL::isa( @_ );
			}
			sub DOES {
				my $self = shift;
				$_[0] eq 'Newtype' or
					$$self->isa( @_ ) or
					$self->UNIVERSAL::DOES( @_ );
			}
			sub can {
				my $self = shift;
				$$self->can( @_ ) or
					$self->UNIVERSAL::can( @_ );
			}
		};
	}
	local $@;
	eval "$perl_code; 1" or die( $@ );
	
	if ( $kind ne 'Object' ) {
		require Sub::HandlesVia::CodeGenerator;
		my $gen = 'Sub::HandlesVia::CodeGenerator'->new(
			target => $class,
			attribute => 'Newtype',
			isa => $inner,
			coerce => $inner->has_coercion(),
			generator_for_self => sub { '$_[0]' },
			generator_for_slot => sub { my ( $g ) = @_; sprintf '${%s}', $g->generate_self },
			generator_for_get => sub { my ( $g ) = @_; $g->generate_slot },
			generator_for_set => sub { my ( $g, $v ) = @_; sprintf '(%s=%s)', $g->generate_slot, $v },
			generator_for_default => sub { 'undef' }, # XXX
			get_is_lvalue => !!1,
			set_checks_isa => !!0,
		);
		my $shv_lib = "Sub::HandlesVia::HandlerLibrary::$kind";
		eval "require $shv_lib; 1" or die( $@ );
		for my $h_name ( $shv_lib->handler_names ) {
			my $h = $shv_lib->get_handler( $h_name );
			$gen->generate_and_install_method( $h_name, $h );
		}
	}

	my %methods = %{ $self->{methods} // {} };
	for my $name ( keys %methods ) {
		no strict 'refs';
		*{"$class\::$name"} = $methods{$name};
	}
}

1;

__END__

=pod

=encoding utf-8

=head1 NAME

Newtype - Perl implementation of an approximation for Haskell's newtype

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 BUGS

Please report any bugs to
L<https://github.com/tobyink/p5-newtype/issues>.

=head1 SEE ALSO

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2022 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

