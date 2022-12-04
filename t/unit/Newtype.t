=pod

=encoding utf-8

=head1 PURPOSE

Unit tests for L<Newtype>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2022 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.


=cut

use Test2::V0 -target => 'Newtype';
use Test2::Tools::Spec;
use Data::Dumper;

use Types::Common qw( HashRef );
use Type::Registry ();

$Data::Dumper::Deparse = 1;

describe "class `$CLASS`" => sub {

	tests 'meta' => sub {

		ok $CLASS->isa( 'Newtype' );
		ok $CLASS->isa( 'Type::Tiny::Class' );
		ok $CLASS->isa( 'Type::Tiny' );
		ok $CLASS->isa( 'Exporter::Tiny' );
	};
};

describe "method `_exporter_fail`" => sub {

	tests 'it works' => sub {

		my %func = $CLASS->_exporter_fail(
			'TestHash',
			{ inner => HashRef },
			{ into  => 'Local::Test1' },
		);

		is( scalar( keys %func ), 4, 'expected number of functions returned' );

		is( ref( $func{TestHash} ), 'CODE', 'got a function called TestHash' );
		is( ref( $func{is_TestHash} ), 'CODE', 'got a function called is_TestHash' );
		is( ref( $func{assert_TestHash} ), 'CODE', 'got a function called assert_TestHash' );
		is( ref( $func{to_TestHash} ), 'CODE', 'got a function called to_TestHash' );

		subtest 'TestHash( $inner_value ) seems to work' => sub {
			my $value = $func{TestHash}->( {} );
			isa_ok( $value, 'Local::Test1::Newtype::TestHash' );
			ok( $value->DOES( 'Hash' ) );
		};

		subtest 'TestHash() seems to work' => sub {
			my $value = $func{TestHash}->();
			isa_ok( $value, 'Type::Tiny' );
			is( $value->class, 'Local::Test1::Newtype::TestHash', 'class attribute' );
			is( $value->inner_type->name, HashRef->name, 'inner_type attribute' );
			is( $value->kind, 'Hash', 'kind attribute' );
		};

		subtest 'is_TestHash( $thing ) seems to work' => sub {
			ok( $func{is_TestHash}->( $func{TestHash}->( {} ) ) );
			ok( ! $func{is_TestHash}->( {} ) );
		};

		subtest 'is_TestHash( $thing ) seems to work' => sub {
			lives { $func{assert_TestHash}->( $func{TestHash}->( {} ) ) };
			my $e = dies { $func{assert_TestHash}->( {} ) };
			like $e, qr/did not pass type constraint/;
		};

		subtest 'to_TestHash( $inner_value ) seems to work' => sub {
			my $value = $func{to_TestHash}->( {} );
			isa_ok( $value, 'Local::Test1::Newtype::TestHash' );
		};
	};
};

done_testing;
