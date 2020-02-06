use Modern::Perl;
use HTML::Tidy;
use open qw(:std :utf8);

my @x = split "\n", qx`find .`;
my $t = HTML::Tidy->new({ output_xhtml => 1, tidy_mark => 0 });

foreach my $a (@x) {
  next unless $a =~ m/\.html$/g;
  my ($u) = $a;
  $u =~ s/^\./http:\/\/www.carrotmuseum.co.uk/g;
  my $c = qx`curl $u`;
  my $o = $t->clean($a, $c);
  open(my $fh, '>', $a) or die $!;
  print $fh $o;
  close $fh;
}
