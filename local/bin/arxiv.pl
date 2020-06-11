#!perl

die 'Usage: arxiv <id>' unless $#ARGV == 0;

use strict;
use warnings;
use feature qw/say/;
use utf8::all;

use LWP::Simple;
use File::Fetch;
use XML::Twig;

my $twig=XML::Twig->new();

$twig->parse(get("http://export.arxiv.org/api/query?search_query=id:$ARGV[0]"));

my $root = $twig->root;
my @entry = $root->children('entry');
for my $entry (@entry) {
    my $title = $entry->first_child_text('title');
    $title =~ y/\n/ /;
    $title =~ s/ +/ /g;

    # arXiv id
    my $id = $entry->first_child_text('id');
    $id =~ s{http://arxiv.org/abs/(.+)v(?:\d)+}{$1};

    # Year
    my $year = $entry->first_child_text('published');
    $year =~ s/^(....).*/$1/;

    # Author list
    my @author = $entry->children_text('author');
    my @processed_authors;
    my $key;
    for my $author (@author) {
        $author =~ /^(.+) ([^ ]+)$/;
        $key .= $2;
        push @processed_authors, "$2, $1";
    }
    my $author_list = join ' and ', @processed_authors;
    $key .= $year;

    my $pdf_url = $entry->first_child('link[@title="pdf"]')->{att}{href};
    my $ff = File::Fetch->new(uri => $pdf_url);
    my $file = $ff->fetch() or die $ff->error();
    rename $file, "$key.pdf";
    my $first = substr $key, 0, 1; # first letter of the key

    printf <<'EOS'
@misc{%s,
  author = {%s},
  title = {%s},
  year = {%s},
  eprint = {%s},
  eprinttype = {arXiv},
  file = {%s},
  pubstate = {prepublished}
}
EOS
      ,
      $key, $author_list, $title, $year, $id, $first . "/$key.pdf";
}
