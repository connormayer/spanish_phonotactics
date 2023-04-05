#!/pkg/bin/perl 
###############################################################################
# This software is being provided to you, the LICENSEE, by the Linguistic     #
# Data Consortium (LDC) and the University of Pennsylvania (UPENN) under the  #
# following license.  By obtaining, using and/or copying this software, you   #
# agree that you have read, understood, and will comply with these terms and  #
# conditions:                                                                 #
#                                                                             #
#      This program is free software; you can redistribute it and/or modify   #
#      it under the terms of the GNU General Public License as published by   #
#      the Free Software Foundation; either version 1, or (at your option)    #
#      any later version.                                                     #
#                                                                             #
#      This program is distributed in the hope that it will be useful,        #
#      but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          #
#      GNU General Public License (GNU_COPYRIGHT) at the same directory       #
#      for more details.                                                      # 
#                                                                             #
# Copyright 1995 by the University of Pennsylvania.  All rights reserved.     #
#                                                                             #
# THIS SOFTWARE IS PROVIDED "AS IS"; LDC AND UPENN MAKE NO REPRESENTATIONS OR #
# WARRANTIES, EXPRESS OR IMPLIED.  By way of example, but not limitation,     #
# LDC AND UPENN MAKE NO REPRESENTATIONS OR WARRANTIES OF MERCHANTABILITY OR   #
# FITNESS FOR ANY PARTICULAR PURPOSE.                                         #
###############################################################################
# spron.pl Version 0.1 Jan. 11 1995 
# Written by Zhibiao Wu, LDC, wzb@unagi.cis.upenn.edu
# This program needs the basic_rules file to run. The rules must be sorted 
# in alphabetical order. The most specific rules should precede the more 
# general ones. The conventions used in the basic rules are the same as 
# regular expressions used in Perl.

# Revised history: Feb. 10 1995

# The file "preferences" (assumed to be in your current directory)
# gives an "oracle" of correct pronunciations that override the
# machine-generated ones.

# slightly changed 97/09/05 robertm:
#  - look for basic_rules and preferences in $PWD instead of ~wzb/...
#  - use next to shortcut loop instead of if/else
#  - added a bit of documentation, without really trying to decipher this thing

$vfile = "";
$print_input = 0;
foreach $i (@ARGV) {
    if (($_ eq $i) || ($i eq /^-/)) {$shift_count++;
    } elsif ($i eq '-m') {$print_input = 1; $shift_count++;
    } else {
      $vfile = $i; $shift_count++;
    } 
}
while ($shift_count-- > 0) {shift(@ARGV);}

$rule_num = 0;
$previous = "";
if ($vfile ne "") {
 open(VF, $vfile) || die "Can't find file $vfile!\n";
 while (<VF>) {
    chop;
    split(//);
    if ((@_[0] ne '#') && ($_ ne "")) {
     if (/(\S+)\s*->\s*(\S*)\s*:\s*(\S*)\s*__\s*(\S*)\s*(#?)/) {
      $head[$rule_num] = $1;
      $end[$rule_num] = $2;
      $pre[$rule_num] = $3;
      if ($4 =~ /#/) {
       $nex[$rule_num] = "";
       $some[$rule_num] = $4;
      } else {
       $nex[$rule_num] = $4;
       $some[$rule_num] = $5;
      }
      if ($previous ne substr($head[$rule_num],0,1)) {
       $first{$head[$rule_num]} = $rule_num;
       $last{$previous} = $rule_num - 1;
      }
      $previous = substr($head[$rule_num++],0,1);
     } else {
      print "Rule format error: Cannot parse $_\n";
      exit(1);
     }
    }
 }
 $last{$previous} = $rule_num - 1;

 close(VF);
}

 open(PF, "./preferences") || die "Can't read `preferences' file";
 while (<PF>) {
  chop;
  if ($_ ne "") {
   split;
   $pron{@_[0]} = @_[1];
   $stre{@_[0]} = @_[2];
  }
 }

$previous = "";
$brule_num = 0;
 open(BF, "./basic_rules") || die "Can't read `basic_rules' file";
 while (<BF>) {
    chop;
    split(//);
    if ((@_[0] ne '#') && ($_ ne "")) {
     if (/(\S+)\s*->\s*(\S*)\s*:\s*(\S*)\s*__\s*(\S*)\s*(#?)/) {
      $bhead[$brule_num] = $1;
      $bend[$brule_num] = $2;
      $bpre[$brule_num] = $3;
      if ($4 =~ /#/) {
       $bnex[$brule_num] = "";
       $bsome[$brule_num] = $4;
      } else {
       $bnex[$brule_num] = $4;
       $bsome[$brule_num] = $5;
      }
      if ($previous ne substr($bhead[$brule_num],0,1)) {
       $bfirst{substr($bhead[$brule_num],0,1)} = $brule_num;
       $blast{$previous} = $brule_num - 1;
      }
      $previous = substr($bhead[$brule_num++],0,1);
     } else {
      print "Rule format error in file basic_rules: Cannot parse $_\n";
      exit(1);
     }
    }
 }
 $blast{$previous} = $brule_num - 1;
 close(BF);

if ($brule_num == 0) {
 print "No basic rules, Program exit!\n";
 exit(1);
}

while(<>){
 next if ((/^#/) || (/^\s*$/) );
 chop;
 if ($print_input) {
  print $_, "\t";
 }
 if ($pron{$_}) {
  # print answer from preferences and skip to next word
  print "$pron{$_}\t$stre{$_}\n";
  next;
 }
 $original = $_;
 tr/A-ZÁÉÍÓÚÏÜÑ/a-záéíóúïüñ/;
 $orig = "#" . $_ . "#";

 @l = ();

 push(@l,split("",$orig));

 @pron = &transfer(1);

 foreach (@pron) {
  $a = $_;
  y/aeiouáéíóú//cd;
  if ($_ eq "") {
   print "#No stressable vowel in $original\n";
  } else {
   s/[aeiou]/0/go;
   s/[áéíóú]/1/go;
   if (!/1/) {
    if(length() == 1){
      s/\b./1/o;
    } elsif($l[$#l - 1] =~ /[aeiouns]/o){
      s/00\b/10/o;
    } else {
      s/0\b/1/o;
    }
   }

   $a =~ s/á/a/g;
   $a =~ s/é/e/g;
   $a =~ s/í/i/g;
   $a =~ s/ó/o/g;
   $a =~ s/ú/u/g;

   print "$a\t$_\n";
  }
 }
}

sub transfer{
 local($_) = @_;
 local(@p) = ();
 local($s) = 0;
 local($over) = 0;
 local($i,$j,$k) = (0,0,0);
 
 if ($_ >= length($orig) - 1) {
  push(@p, "");
  return(@p);
 } else {
  
  if ($vfile ne "") {
   for ($i=   $first{substr($orig, $_, 1)}; 
        $i <= $last{substr($orig, $_, 1)} ; $i++) {
    if (&matchv($_,$i)) {
     $s = $_ + length($head[$i]);
     foreach $w (&transfer($s)) {
      push(@p, $end[$i] . $w);
      if ($some[$i] ne "") {
       $over = 0;
      } else {
       $over = 1;
      }
     }
    }
   }
  }

  if ($over == 0 ) {
   $i = $bfirst{substr($orig, $_, 1)}; 
   while (($i <= $blast{substr($orig, $_, 1)}) && ($over == 0)) {
    if (&matchb($_,$i)) {
     $over = 1;
     $s = $_ + length($bhead[$i]);
     foreach $w (&transfer($s)) {
      push(@p, $bend[$i] . $w);
     }
    }
    $i++;
   }
   if ($over == 0) {
    $s = $_ + 1;
    foreach $w (&transfer($s)) {
     push(@p, substr($orig,$_,1) . $w);
    }
   } 
  }
 
 return(@p);
 }
}

sub matchv {
 $h = $head[@_[1]];
 $p = $pre[@_[1]];
 $n = $nex[@_[1]];

 return(&match(@_[0],$h,$p,$n));

}

sub matchb {
 $h = $bhead[@_[1]];
 $p = $bpre[@_[1]];
 $n = $bnex[@_[1]];

 return(&match(@_[0],$h,$p,$n));

}

sub match {

 if (substr($orig, @_[0], length(@_[1])) eq @_[1]) {
  return ( &match_n(@_[0] + length(@_[1]) - 1, @_[3]) && 
           &match_p(@_[0], @_[2])); 
 } else {
  return (0);
 }
}

sub match_p {
 local($a) = @_[0];
 local($b) = @_[1];
 local($_);

 if ($b eq "" ) {
  return (1);
 } else {
   $_ = substr($orig, 0, $a) . "!";  
   if (/($b)!/) {
    return(1);
   } else {
    return(0);
  }
 }
}

sub match_n {
 local($a) = @_[0];
 local($b) = @_[1];
 local($_);

 if ($b eq "" ) {
  return (1);
 } else {
   $_ = "!" . substr($orig, $a + 1, length($orig) - $a - 1);  
   if (/!($b)/) {
    return(1);
   } else {
    return(0);
  }
 }
}




