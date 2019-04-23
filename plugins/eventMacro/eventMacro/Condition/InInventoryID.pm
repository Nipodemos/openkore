package eventMacro::Condition::InInventoryID;

use strict;

use base 'eventMacro::Condition::Base::InInventory';

use eventMacro::Utilities qw( get_inventory_amount );

sub _parse_syntax {
	my ( $self, $condition_code ) = @_;
	
	$self->{wanted} = undef;
	
	if ($condition_code =~ /^(\d+)\s+(\S.*)$/) {
		$self->{wanted} = $1;
		$condition_code = $2;
	} else {
		$self->{error} = "Item ID must and a numeric comparison must be given";
		return 0;
	}
	
	$self->SUPER::_parse_syntax($condition_code);
}

sub _get_val {
	my ( $self ) = @_;
	return get_inventory_amount($self->{wanted});
}

1;
