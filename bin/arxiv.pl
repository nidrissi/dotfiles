#!/usr/bin/perl

use strict;
use warnings;
use feature qw/say/;

use LWP::Simple;
use XML::Twig;
use Date::Parse;

my $twig=XML::Twig->new();

die 'Usage: arxiv.pl arxiv_id' unless $#ARGV == 0;
$twig->parse(get("http://export.arxiv.org/api/query?search_query=id:$ARGV[0]"));

my $root = $twig->root;
my @entry = $root->children('entry');
for my $entry (@entry) {
    my $title = $entry->first_child('title')->text;

    # arXiv id
    my $id = $entry->first_child('id')->text;
    $id =~ s{http://arxiv.org/abs/(.+)v(?:\d)+}{$1};

    # Year
    my $date = $entry->first_child('published')->text;
    $date =~ s/^(....).*/$1/;

    # Author list
    my @author = map {
        # Najib Idrissi -> Idrissi, Najib
        # Assumes only the last word is the surname
        $_->text =~ s/^(.+) ([^ ]+)$/$2, $1/r;
    } $entry->children('author');
    my $author_list = join ' and ', @author;

    printf '@misc{tmpkey, author = {%s}, title = {%s}, date = {%s}, eprint = {%s}, eprinttype = {arXiv}, note = {Preprint}}',
      $author_list, $title, $date, $id;
    print "\n";
}
