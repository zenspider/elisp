#!/bin/sh -- # -*- perl -*-
eval 'exec perl -w -S $0 ${1+"$@"}'
  if 0;

# Based on Template.pm version 1.1
# $Id: //depot/main/user/ryand/Bin/elisp/autoinsert/perl-insert.pm#1 $

package PackageName;
use strict;
require 5.004;

=head1 NAME

ModuleName - brief desc

=head1 SYNOPSIS

  use PackageName;
  
  # class methods
  $ob = PackageName->new;
  
  # object methods
  $result = $ob->method;

=head1 DESCRIPTION

The ModuleName class implements ...

=cut

BEGIN {
  use Exporter   ();
  use vars       qw($VERSION
		    @ISA
		    @EXPORT
		    @EXPORT_OK
		    );
  
  # set the version for version checking
  $VERSION     = 1.00;
  
  @ISA         = qw(Exporter);

  # Your required exported functions:
  @EXPORT      = qw(
		    &meth1
		    &meth2
		    &meth4
		    );

  # Your optionally exported functions:
  @EXPORT_OK   = qw(
		    $Var1
		    %%Hashit
		    &meth3
		    );
}

# non-exported package globals go here
use vars      qw(@EXPORT_OK
		 @more 
		 $stuff
		 $AUTOLOAD
		 %FIELD_PRIVATE
		 %FIELD_READ_ONLY
		 );

# initalize package globals, first exported ones
$Var1   = '';
%%Hashit = ();

# then the others (which are still accessible as $Some::Module::stuff)
$stuff  = '';
@more   = ();

# all file-scoped lexicals must be created before
# the functions below that use them.

# file-private lexicals go here
my $priv_var    = '';
my %%secret_hash = ();

# here's a file-private function as a closure,
# callable as &$priv_func;  it cannot be prototyped.
my $priv_func = sub {
  # stuff goes here.
};

=head2 Methods

=over 4

=item $p = PackageName->new(@args);

Description.

=cut

sub new {
  my ($class, $path) = @_;

  my $self = {};
  bless($self, $class);

  # Set the field values:
  $self->{FIELD} = "";

  return $self;
}

=item Generic Accessor ($self->var_name) or ($self->var_name(val))

Provides accessors for predefined fields including read-only and private protection for specified fields.

=over 4

=item fieldName (public/private, read-write/read-only)

=cut

sub AUTOLOAD {
    my $self = shift;
    my $attr = $AUTOLOAD;
    $attr =~ s/.*:://;

    my $ATTR = uc $attr;

    # skip DESTROY and all-cap methods.
    return if $attr =~ /^[A-Z_]$/;

    # die if accessor isn't defined.
    croak "invalid attribute method: $attr()"
      unless defined($self->{$ATTR});

    # die if read only and trying to write.
    croak "read only accessor: $attr()"
      if $field_read_only{$ATTR} and @_;

    # Gets the subroutine's package name.
    # If called directly, assume main
    my ($you, $me);
    $me = (caller(0))[3];
    $you = (caller(1))[3] || "";

    $me  =~ s/::.*//;
    $you =~ s/::.*//;;

    # die if external routine accessing private accessor
    croak "private accessor: $attr() $me ne $you\n"
      if ($field_private{$ATTR} 
	  and ($me ne $you
	       or ! defined $you));

    $attr = uc($attr);
    $self->{$attr} = shift if @_;
    return $self->{$attr};
}

=item $result = $self->meth1();

Description.

=cut

sub meth1 {

  my $self = shift;

  return 0;
}

=item $result = selftest();

Run all known tests against module.

=cut

sub selftest {

  print "Starting tests...\n";
  # Self tests here
  
  print "...done\n";

  return 0;
}

=item $result = END();

Global destructor for module.

=cut

END {

  my $self = shift;

  return 0;
}

1;
__END__

=back

=head1 EXAMPLES

  example

=head1 AUTHOR

Created: %y-%M-%D by %U <%a>

=cut
