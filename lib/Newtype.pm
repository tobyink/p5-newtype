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

sub kind       { $_[0]{kind} }
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
				my ( $self, $c ) = @_;
				$c = $c->class if Scalar::Util::blessed($c) && $c->can('class');
				$$self->isa( $c ) or
					$self->UNIVERSAL::isa( $c );
			}
			sub DOES {
				my ( $self, $r ) = @_;
				$r = $r->class if Scalar::Util::blessed($r) && $r->can('class');
				$r eq 'Newtype' or
					$$self->isa( $r ) or
					$self->UNIVERSAL::DOES( $r );
			}
			sub can {
				my ( $self, $m ) = @_;
				$$self->can( $m ) or
					$self->UNIVERSAL::can( $m );
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

  package MyClass;
  
  use HTTP::Tiny ();
  use Newtype HttpTiny => { inner => 'HTTP::Tiny' };
  
  use Moo;
  
  has ua => (
    is => 'ro',
    isa => HttpTiny(),
    coerce => 1,
  );

=head1 DESCRIPTION

This module allows you to create a new type which is a subclass of an existing
type.

Why?

Well maybe you want to add some new methods to the new type:

  use HTTP::Tiny ();
  use Newtype HttpTiny => {
    inner => 'HTTP::Tiny',
    methods => {
      'post_or_get' => sub {
        my $self = shift;
        my $res = $self->post( @_ );
        return $res if $res->{success};
        return $self->get( @_ );
      },
  };

Or maybe you need to differentiate between two different kinds of things
which are otherwise the same class.

  use Newtype (
    SecureUA    => { inner => 'HTTP::Tiny' },
    InsecureUA  => { inner => 'HTTP::Tiny' },
  );
  
  ...;
  
  my $ua = InsecureUA( HTTP::Tiny->new );
  
  ...;
  
  if ( $ua->isa(SecureUA) ) {
    ...;
  }

Newtype can also create new types which "inherit" from Perl builtins.

  use Types::Common qw( ArrayRef PositiveInt );
  use Newtype Numbers => { inner => ArrayRef[PositiveInt] };
  
  my $nums = Numbers( [] );
  $nums->push(  1 );
  $nums->push(  2 );
  $nums->push( -1 );  # dies

See L<Hydrogen> for the list of available methods for builtins.

=head2 Creating a newtype

The general form for creating newtypes is:

  use Newtype $typename => {
    inner => $inner_type,
    %other_options,
  };

The inner type is required, and must be either a string class name or
a L<Type::Tiny> type constraint indicating what type of thing you want
to wrap.

Other supported options are:

=over

=item C<methods>

A hashref of methods to add to the newtype. Keys are the method names.
Values are coderefs.

=item C<coercions>

(TODO)

=item C<kind>

This allows you to give Newtype a hint for how to delegate to the inner
value. Supported kinds (case-sensitive) are: Array, Bool, Code, Counter,
Hash, Number, Object, and String. Usually Newtype will be able to guess
based on C<inner> though.

=back

=head2 Creating values belonging to the newtype

When you import a newtype B<Foo>, you import a function C<< Foo() >>
into your namespace. You can create instances of the newtype using:

  Foo( $inner_value )

Where C<< $inner_value >> is an instance of the thing you're wrapping.

For example:

  use HTTP::Tiny;
  use Newtype UA => { inner => 'HTTP::Tiny' };
  
  my $ua = UA( HTTP::Tiny->new );

I<< Note: >> you also get C<is_Foo>, C<assert_Foo>, and C<to_Foo>
functions imported! C<< is_Foo( $x ) >> checks if C<< $x >> is a B<Foo>
object and returns a boolean. C<< assert_Foo( $x ) >> does the same,
but dies if it fails. C<< to_Foo( $x ) >> attempts to coerce C<< $x >>
to a B<Foo> object.

=head2 Integration with Moose, Mouse, and Moo

If your imported newtype is B<Foo>, then calling C<< Foo() >> with no
arguments will return a L<Type::Tiny> type constraint for the newtype.

  use HTTP::Tiny;
  use Newtype UA => { inner => 'HTTP::Tiny' };
  
  use Moo;
  has my_ua => ( is => 'ro', isa => UA() );

Now people instantiating your class will need to pass you a wrapped
HTTP::Tiny object instead of passing a normal HTTP::Tiny object. You may
wish to allow them to pass you a normal HTTP::Tiny object though.
That should be easy with coercions:

  has my_ua => ( is => 'ro', isa => UA(), coerce => 1 );

=head2 Accessing the inner value

You can access the original wrapped value using the C<< INNER >> method.

  my $ua = UA( HTTP::Tiny->new );
  my $http_tiny_object = $ua->INNER;

=head2 Introspection

If your newtype is called B<MyNewtype>, then you can introspect it using
a few methods:

=over

=item C<< MyNewtype->class >>

The class powering the newtype.

=item C<< MyNewtype->inner >>

The type constraint for the inner value.

=item C<< MyNewtype->kind >>

The kind of delegation being used.

=back

The object returned by C<< MyNewtype() >> is also a L<Type::Tiny> object,
so you can call any method from L<Type::Tiny>, such as
C<< MyNewtype->check( $value ) >>.

=head1 BUGS

Please report any bugs to
L<https://github.com/tobyink/p5-newtype/issues>.

=head1 SEE ALSO

L<Type::Tiny::Class>, L<Subclass::Of>.

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

