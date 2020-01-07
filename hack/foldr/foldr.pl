use strict;
use warnings;
use Modern::Perl;
sub thunk {
  my ($val,$list) = @_;
  my $thunk;
  $thunk = sub {
    if($list && ref $list eq 'ARRAY'){
      push @$list, $thunk;
    }
    $val;
  };
  $thunk;
};
sub cycle {
  my ($val) = @_;
  my @rval;
  push @rval, thunk($val, \@rval);
  \@rval;
}
sub step_returns {
  my ($x, $a) = @_;
  $x->() || $a->();
}
sub step_inf {
  my ($x, $a) = @_;
  $a->() || $x->();
}
sub foldr {
  my ($f, $z, $list) = @_;
  return $z unless (@$list);
  $f->($list->[0], sub {
    splice @$list, 1;
    foldr($f, $z, $list);
  });
}
my $list = cycle("0e0");
say foldr(\&step_returns, 0, $list) ? "true" : "false";
say foldr(\&step_inf, 0, $list) ? "true" : "false";
