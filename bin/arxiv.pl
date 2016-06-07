use strict;
use warnings;
use feature qw/say/;

use LWP::Simple;
use XML::Twig;
use Date::Parse;

my $twig=XML::Twig->new();

die 'Usage: foo.pl arxiv_id' unless $#ARGV == 0;
$twig->parse(get("http://export.arxiv.org/api/query?search_query=id:$ARGV[0]"));

my $root = $twig->root;
my @entry = $root->children('entry');
for my $entry (@entry) {
    my $title = $entry->first_child('title')->text;

    my $id = $entry->first_child('id')->text;
    $id =~ s{http://arxiv.org/abs/(.+)v..?}{$1};

    my $date = $entry->first_child('published')->text;
    $date =~ s/^(....).*/$1/;

    my @author;
    for my $auth ($entry->children('author')) {
        $auth = $auth->text;
        $auth =~ s/^(.+) ([^ ]+)$/$2, $1/;
        push @author, $auth;
    }
    my $author_list = join ' and ', @author;

    my $category = $entry->first_child('arxiv:primary_category')->{'att'}{'term'};

    printf '@misc{tmpkey, author = {%s}, title = {%s}, date = {%s}, eprint = {%s}, eprinttype = {arXiv}, eprintclass = {%s}, note = {Preprint}}',
      $author_list, $title, $date, $id, $category;
    say '';
}
