#!/usr/bin/perl

use strict;
use warnings;

my $out_f;
open $out_f, ">src/RuntimeSource.hs";

print $out_f "module RuntimeSource where\n\nimport Data.String\n\n";

for my $filepath (<"res/*.js">) {
    my $f;
    open $f, $filepath;
    local $/;
    my $src = <$f>;
    close $f;

    my($name) = $filepath =~ m!/(.*)\.!;
    $name =~ s/(?:^|-)([a-z])/uc $1/ge;
    print $out_f "gen$name :: IsString a => a\n";

    $src = $src =~ s/\\/\\\\/gr =~ s/"/\\"/gr =~ s/\r?\n/\\n\\\n\\/gr =~ s/\\\n\\\z//r;
    print $out_f qq(gen$name = "$src"\n\n);
}

for my $filepath (<"res/*.hs">) {
    my $f;
    open $f, $filepath;
    local $/;
    my $src = <$f>;
    close $f;

    my($name) = $filepath =~ m!/(.*)\.!;
    $name =~ s/(?:^|-)([a-z])/uc $1/ge;
    print $out_f "src$name :: IsString a => a\n";

    $src = $src =~ s/\\/\\\\/gr =~ s/"/\\"/gr =~ s/\r?\n/\\n\\\n\\/gr =~ s/\\\n\\\z//r;
    print $out_f qq(src$name = "$src"\n\n);
}

close $out_f;
