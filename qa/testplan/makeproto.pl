#!/usr/bin/perl
# $Id: makeproto.pl 1136 2007-05-15 11:38:15Z podolsir $

my $testplan = 'testplan.tex';
my $prototpl = 'test-protocol-template.tex';

use strict;
open TESTPLAN, '<', $testplan;

my $testcases = {};
my $lastID;
my $testplanversion;
my $testplansvnrev;
while (my $line = <TESTPLAN>) {
    
    if ($line =~ /\{testcase\}\{(.*?)\}{(.*?)}{(.*?)}/) {
	my ($id, $desc, $type) = ($1, $2, $3);
	$lastID = $id;
	if (defined($testcases->{$id})) {
	    print "WARNING: test case $id doubly defined, skipping the new one";
	    next;
	}
	$testcases->{$id} = {id => $id,
			     desc => $desc,
			     type => $type};
    }

    if ($line =~ /env\{(.*?)\}/) {
	my @env = split(/,\s*/, $1);
	$testcases->{$lastID}->{'env'} = \@env;
    }

    if ($line =~ /lang\{(.*?)\}/) {
	my @lang = split(/,\s*/, $1);
	$testcases->{$lastID}->{'lang'} = \@lang;
    }

    if (!$testplanversion && $line =~ /Version:\s*(.*?)\s/) {
	$testplanversion = $1;
    }

    if (!$testplansvnrev && $line =~ /Id:\s*[^\s]*?\s*(\d+)\s/) {
	$testplansvnrev = $1;
    }
}
close TESTPLAN;

foreach my $tc (values(%$testcases)) {
    if (!defined($tc->{'env'})) {
	$tc->{'env'} = ['~'];
    }
    if (!defined($tc->{'lang'})) {
	$tc->{'lang'} = ['~'];
    }
}


open TPL, '<', $prototpl;

my $template;
while (my $line = <TPL>) {
    $template .= $line;
}

close TPL;
foreach my $tc (sort {$a->{'id'} cmp $b->{'id'}} values(%$testcases)) {
    foreach my $lang (@{$tc->{'lang'}}) {
	foreach my $env (@{$tc->{'env'}}) {
	    $template =~ s/\%TESTCASE/$tc->{id} & $lang & $env & $tc->{desc} & & & &\\\\\n\\hline\n\%TESTCASE/;
	}
     }
}
$template =~ s/%TESTPLANVERSION/$testplanversion (SVN Rev $testplansvnrev)/;
print $template;
