#!/usr/bin/env perl

use strict;
use warnings;
use Module::Build;

my %requires = (
	'Symbol' => 0,
	'Encode' => 0,
	'Fcntl'  => 0,
);

if ($^O =~ /MSWin/) {
    $requires{"Win32::API"}           = 0;
    $requires{"Win32API::File"}       = 0;
    $requires{"Win32API::File::Time"} = 0;
} else {
    $requires{"File::Copy"}           = 0;
    $requires{"File::Path"}           = 0;
}

my $build = Module::Build->new (
    module_name       => "PortIO",
    dynamic_config    => "1",
    license           => "bsd",
    requires          => \%requires,
);

$build->create_build_script;
