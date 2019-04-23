# $Id: Utilities.pm r6812 2009-07-29 14:00:00Z ezza $
package eventMacro::Utilities;

use strict;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(q4rx q4rx2 between compare_arguments match_regex get_args get_npc_binID get_player_binID
	get_monster_binID get_vender_binID get_item_binIDs getItemPrice get_inventory_binIDs get_inventory_type_binIDs get_storage_binIDs get_sold_out get_inventory_amount
	get_cart_amount get_shop_amount get_storage_amount get_vend_amount get_random get_random_range get_config
	get_word call_macro get_arg_from_list get_list_lenght same_party processCmd find_variable get_key_or_index
	get_quest_status get_pattern find_hash_and_get_keys find_hash_and_get_values);

use Utils;
use Globals;
use AI;
use Log qw(message error warning debug);
use Utils qw(parseArgs);

use eventMacro::Core;
use eventMacro::Data;
use eventMacro::Lists;
use eventMacro::Automacro;
use eventMacro::FileParser;

sub between {
	my ($min, $value, $max) = @_;

	if ($min <= $value && $value <= $max) {
		return 1;
	} else {
		return 0;
	}
}

sub compare_arguments {
	my ($first, $cond, $second) = @_;
	
	if (defined $first && !defined $cond & !defined $second) {
		if ($first eq '' || $first == 0) {
			return 0;
		} else {
			return 1;
		}
		
	} elsif ($first =~ /^\s*(-?[\d.]+)\s*\.{2}\s*(-?[\d.]+)\s*$/) {
		my ($first1, $first2) = ($1, $2);
		if ($second =~ /^-?[\d.]+$/) {
			if ($cond eq "!=") {
				return ((between($first1, $second, $first2)) ? 0 : 1);
				
			} elsif ($cond eq "=" || $cond eq "==" || $cond eq "=~" || $cond eq "~") {
				return between($first1, $second, $first2);
				
			} else {
				error "compare_arguments: Range operations must be of equality or inequality\n", "eventMacro";
				return;
			}
		}
		error "compare_arguments: wrong # of arguments ($first) ($cond) ($second)\n--> ($second) <-- maybe should be numeric?\n", "eventMacro";
		return;
		
	} elsif ($second =~ /^\s*(-?[\d.]+)\s*\.{2}\s*(-?[\d.]+)\s*$/) {
		my ($second1, $second2) = ($1, $2);
		if ($first =~ /^-?[\d.]+$/) {
			if ($cond eq "!=") {
				return ((between($second1, $first, $second2)) ? 0 : 1);
				
			} elsif ($cond eq "=" || $cond eq "==" || $cond eq "=~" || $cond eq "~") {
				return between($second1, $first, $second2);
				
			} else {
				error "compare_arguments: Range operations must be of equality or inequality\n", "eventMacro";
				return;
			}
		}
		error "compare_arguments: wrong # of arguments ($first) ($cond) ($second)\n--> ($first) <-- maybe should be numeric?\n", "eventMacro";
		return;
		
	} elsif ($first =~ /^-?[\d.]+$/ && $second =~ /^-?[\d.]+$/) {
		return ($first == $second ? 1 : 0) if (($cond eq "=" || $cond eq "=="));
		return ($first >= $second ? 1 : 0) if ($cond eq ">=");
		return ($first <= $second ? 1 : 0) if ($cond eq "<=");
		return ($first >  $second ? 1 : 0) if ($cond eq ">");
		return ($first <  $second ? 1 : 0) if ($cond eq "<");
		return ($first != $second ? 1 : 0) if ($cond eq "!=");
		
	} elsif (($cond eq "=" || $cond eq "==")) {
		return ($first eq $second ? 1 : 0);
		
	} elsif ($cond eq "!=") {
		return ($first ne $second ? 1 : 0);
		
	} elsif ($cond eq "~") {
		$first = lc($first);
		foreach my $member (split(/\s*,\s*/, $second)) {
			return 1 if ($first eq lc($member));
		}
		
	} elsif ($cond eq "=~" && $second =~ /^\/.*?\/\w?\s*$/) {
		return match_regex($first, $second);
	}

	return 0;
}

sub q4rx {
	my $s = $_[0];
	$s =~ s/([\/*+(){}\[\]\\\$\^?])/\\$1/g;
	return $s
}

sub q4rx2 {
	# We let alone the original q4rx sub routine... 
	# instead, we use this for our new &nick ;p
	my $s = $_[0];
	$s =~ s/([\/*+(){}\[\]\\\$\^?"'\. ])/\\$1/g;
	return $s
}

sub match_regex {
	my ($text_to_be_compared, $regex) = @_;

	unless (defined $text_to_be_compared && defined $regex) {
		# this produces a warning but that's what we want
		error "match_regex: wrong # of arguments ($text_to_be_compared) ($regex)\n", "eventMacro";
		return;
	}

	if ($regex =~ /^\/(.*?)\/(\w?)$/) {
		if ($text_to_be_compared =~ /$1/ || ($2 eq 'i' && $text_to_be_compared =~ /$1/i)) {
			no strict;
			foreach my $idx (1..$#-) {$eventMacro->set_scalar_var(".lastMatch$idx",${$idx})}
			use strict;
			return 1;
		} else {
			return 0;
		}
	}

	return;
}

sub get_args {
	my $arg = $_[0];
	if ($arg =~ /".*"/) {
		my @ret = $arg =~ /^"(.*?)"\s+(.*?)( .*)?$/;
		$ret[2] =~ s/^\s+//g if defined $ret[2];
		return @ret
	} else {
		return split(/\s/, $arg, 3)
	}
}

# gets word from message
sub get_word {
	my ($message, $wordno) = $_[0] =~ /^"(.*?)"\s*,\s?(\d+|\$[a-zA-Z][a-zA-Z\d]*)$/s;
	my @words = split(/[ ,.:;\"\'!?\r\n]/, $message);
	my $no = 1;
	if ($wordno =~ /^\$/) {
		my ($val) = $wordno =~ /^\$([a-zA-Z][a-zA-Z\d]*)\s*$/;
		return "" unless defined $val;
		if ($eventMacro->get_scalar_var($val) =~ /^[1-9][0-9]*$/) {$wordno = $eventMacro->get_scalar_var($val)}
		else {return ""}
	
	}
	foreach (@words) {
		next if /^$/;
		return $_ if $no == $wordno;
		$no++
	}
	return ""
}

# gets openkore setting
sub get_config {
	my ($arg) = @_;
	$arg =~ s/^\s+|\s+$//g;
	# Basic Support for "label" in blocks. Thanks to "piroJOKE" (from Commands.pm, sub cmdConf)
	if ($arg1 =~ /\./) {
		$arg1 =~ s/\.+/\./; # Filter Out Unnecessary dot's
		my ($label, $param) = split /\./, $arg1, 2; # Split the label form parameter
		foreach my $key (keys %config) {
			if ($key =~ /_\d+_label/){ # we only need those blocks witch have labels
				if ($::config{$key} eq $label) {
					my ($real_key, undef) = split /_label/, $key, 2;
					# "<label>.block" param support. Thanks to "vit"
					if ($param ne "block") {
						$real_key .= "_";
						$real_key .= $param;
					}
					$arg1 = $real_key;
					last;
				};
			};
		};
	};
	return (defined $::config{$arg1})?$::config{$arg1}:"";
}

# get quest status
# returns a hash of { id => status }, where status is:
#   'inactive'    - if the quest doesn't exist or is not active
#   'incomplete'  - if the quest has a timer which hasn't timed out, or incomplete kill missions
#   'complete'    - if the quest is active and both timer and missions (if any) are complete
sub get_quest_status {
	my @quest_ids = @_;
	my $result = {};
	foreach my $quest_id ( @quest_ids ) {
		my $quest = $questList->{$quest_id};
		if ( !$quest || !$quest->{active} ) {
			$result->{$quest_id} = 'inactive';
		} elsif ( $quest->{time} && $quest->{time} > time ) {
			$result->{$quest_id} = 'incomplete';
        } elsif ( grep { $_->{goal} && $_->{count} < $_->{goal} } values %{ $quest->{missions} } ) {
			$result->{$quest_id} = 'incomplete';
        } elsif ( grep { !$_->{goal} && $_->{count} == 0 } values %{ $quest->{missions} } ) {
			$result->{$quest_id} = 'incomplete';
		} else {
			$result->{$quest_id} = 'complete';
		}
	}
	$result;
}

# get NPC array index
sub get_npc_binID {
	my ($arg) = @_;
	my ($what, $x, $y,$regex,$case_insensitive,$string);

	if ($arg =~ /^\s*(\d+) (\d+)\s*$/) {
		$x = $1;
		$y = $2;

	} elsif ($arg =~ /^\s*\/(.+?)\/(\w?)\s*$/) {
		$regex = $1;
		$case_insensitive = $2;

	} elsif ($arg =~ /^\s*"(.*?)"\s*$/) {
		$string = $1;

	} else {
		return -1;
	}
	
	my @binIDs;
	foreach my $npc (@{$npcsList->getItems()}) {
		if (defined $x && defined $y) {
			return $npc->{binID} if ($npc->{pos}{x} == $x && $npc->{pos}{y} == $y);

		} elsif (defined $regex) {
			if ($npc->{name} =~ /$regex/ || ($case_insensitive eq "i" && $npc->{name} =~ /$regex/i)) {
				push @binIDs, $npc->{binID};
			}

		} else {
			return $npc->{binID} if $npc->{name} eq $string;
		}
	}
	if (@binIDs) {
		return join ',', @binIDs;
	}
	return -1;
}

# get player array index
sub get_player_binID {
	foreach my $pl (@{$playersList->getItems()}) {
		return $pl->{binID} if $pl->name eq $_[0]
	}
	return -1;
}

# get monster array index
sub get_monster_binID {
	foreach my $ml (@{$monstersList}) {
		return $ml->{binID} if ($ml->name eq $_[0] || $ml->{binType} eq $_[0] || $ml->{name_given} eq $_[0]);
	}
	return -1;
}

# get vender array index
sub get_vender_binID {
	for (my $i = 0; $i < @::venderListsID; $i++) {
		next if $::venderListsID[$i] eq "";
		my $player = Actor::get($::venderListsID[$i]);
		return $i if $player->name eq $_[0]
	}
	return -1;
}

# get inventory item binIDs
# checked and ok
sub get_inventory_binIDs {
	my($item_to_find) = @_
	return unless $char->inventory->isReady();
	my $item_to_find = lc($_[0]);
	my @ids;
	foreach my $item (@{$char->inventory->getItems}) {
		if (lc($item->name) eq $item_to_find || $item->{nameID} == $item_to_find) {
			push @ids, $item->{binID}
		}
	}
	unless (@ids) {
		push @ids, -1;
	}
	return @ids;
}

# get inventory item type binIDs
# checked and ok
sub get_inventory_type_binIDs {
	return unless $char->inventory->isReady();
	my $find = lc($_[0]);
	my @ids;
	foreach my $item (@{$char->inventory->getItems}) {
        if ( $item->usable() eq 1 && $find eq "usable")                              { push @ids, $item->{binID} }
        if ( $item->equippable() eq 1 && $item->{equipped} eq 0 && $find eq "equip") { push @ids, $item->{binID} }
        if ( $item->usable() eq 0 && $item->equippable() eq 0 && $find eq "etc" )    { push @ids, $item->{binID} }
        if ( $item->{type} eq 6 && $find eq "card" )                                 { push @ids, $item->{binID} }
    }
	unless (@ids) {push @ids, -1}
	return @ids
}

# get item array index
sub get_item_binIDs {
	my ($item, $pool) = (lc($_[0]), $_[1]);
	return if !$pool->isReady;
	my @ids = map { $_->{binID} } grep { $item eq lc $_->name || $item == $_->{nameID} } @$pool;
	push @ids, -1 if !@ids;
	@ids;
}

# get item price from its index
# works with &venderprice
# returns -1 if no shop is being visited
sub getItemPrice {
	my ($itemIndex, $pool) = ($_[0], $_[1]);
	my $price = -1;
	if ($$pool[$itemIndex]) {$price = $$pool[$itemIndex]{price}}
	return $price
}

# get storage array index
# returns -1 if no matching items in storage
sub get_storage_binIDs {
	return unless $char->storage->wasOpenedThisSession();
	my $find = lc($_[0]);
	my @ids;
	foreach my $item (@{$char->storage->getItems}) {
		if (lc($item->name) eq $find|| $item->{nameID} == $find) {push @ids, $item->{binID}}
	}
	unless (@ids) {push @ids, -1}
	return @ids
}

# get amount of sold out slots
sub get_sold_out {
	return 0 unless $shopstarted;
	my $soldout = 0;
	foreach my $article_item (@::articles) {
		next unless $article_item;
		if ($article_item->{quantity} == 0) {
			$soldout++;
		}
	}
	return $soldout;
}

# get amount of an item in inventory
sub get_inventory_amount {
	my $arg = lc($_[0]);
	return -1 unless ($char->inventory->isReady());
	my $amount = 0;
	foreach my $item (@{$char->inventory->getItems}) {
		if (lc($item->name) eq $arg || $item->{nameID} == $arg) {$amount += $item->{amount}}
	}
	return $amount;
}

# get amount of an item in cart
sub get_cart_amount {
	my $arg = lc($_[0]);
	return -1 unless ($char->cart->isReady());
	my $amount = 0;
	foreach my $item (@{$char->cart->getItems}) {
		if (lc($item->name) eq $arg || $item->{nameID} == $arg) {$amount += $item->{amount}}
	}
	return $amount;
}

# get amount of an item in your shop
sub get_shop_amount {
	my $arg = lc($_[0]);
	my $amount = 0;
	foreach my $articleItem (@::articles) {
		next unless $articleItem;
		if (lc($articleItem->{name}) eq $arg || $articleItem->{nameID} == $arg) {$amount += $articleItem->{quantity}}
	}
	return $amount;
}

# get amount of an item in storage
# returns -1 if the storage is closed
sub get_storage_amount {
	my $arg = lc($_[0]);
	return -1 unless ($char->storage->wasOpenedThisSession());
	my $amount = 0;
	foreach my $item (@{$char->storage->getItems}) {
		if (lc($item->name) eq $arg || $item->{nameID} == $arg ) {$amount += $item->{amount}}
  	}
	return $amount
}

# get amount of items for the specifical index in another venders shop
# returns -1 if no shop is being visited
sub get_vend_amount {
	my ($itemIndex, $pool) = ($_[0], $_[1]);
	my $amount = -1;
	if ($$pool[$itemIndex]) {$amount = $$pool[$itemIndex]{amount}}
	return $amount
}

# returns random item from argument list
sub get_random {
	my $arg = $_[0];
	my @items;
	my $id = 0;
	while (($items[$id++]) = $arg =~ /^[, ]*"(.*?)"/) {
		$arg =~ s/^[, ]*".*?"//g;
	}
	pop @items;
	unless (@items) {
		warning "[eventMacro] wrong syntax in \@random\n", "eventMacro";
		return
	}
	return $items[rand $id-1]
}

# returns given argument from a comma separated list
# returns -1 if no such listID exists or when the list is empty or wrong
sub get_arg_from_list {
	my ($listID, $list) = split(/, \s*/, $_[0]);
	my @items = split(/,\s*/, $list);
	unless (@items) {
		warning "[eventMacro] wrong syntax in \@listItem\n", "eventMacro";
		return -1
	}
	if ($items[$listID]) {
	return $items[$listID]
		} else {
		warning "[eventMacro] the $listID number item does not exist in the list\n", "eventMacro";
		return -1
	}
}

# returns the length of a comma separated list
sub get_list_lenght {
	my $list = $_[0];
	my @items = split(/,\s*/, $list);
	return scalar(@items)
}

# check if player is in party
sub same_party {
	my $player = shift;
	for (my $i = 0; $i < @partyUsersID; $i++) {
		next if $partyUsersID[$i] eq "";
		next if $partyUsersID[$i] eq $accountID;
		return 1 if $char->{'party'}{'users'}{$partyUsersID[$i]}{'name'} eq $player
	}
	return 0
}

# returns random number within the given range  ###########
sub get_random_range {
	my ($value) = @_;
	my ($low, $high) = split(/,\s*/, $value);
	return int(rand($high-$low+1))+$low if (defined $high && defined $low);
}

sub get_pattern {
	my ($inside_brackets) = @_;
	my ($pattern, $var_str) = parseArgs($inside_brackets, undef, ',');
	$var_str =~ s/^\s+|\s+$//gos;
	return $pattern, $var_str;
}

sub find_variable {
	my ($text) = @_;
	
	if (my $scalar = find_scalar_variable($text)) {
		return ({ display_name => $scalar->{display_name}, type => 'scalar', real_name => $scalar->{real_name} });
	}
	
	if (my $array = find_array_variable($text)) {
		return ({ display_name => $array->{display_name},  type => 'array',  real_name => $array->{real_name} });
	}
	
	if (my $hash = find_hash_variable($text)) {
		return ({ display_name => $hash->{display_name},   type => 'hash',   real_name => $hash->{real_name} });
	}
	
	if (my $accessed_var = find_accessed_variable($text)) {
		return ({ display_name => $accessed_var->{display_name}, type => $accessed_var->{type}, real_name => $accessed_var->{real_name}, complement => $accessed_var->{complement} });
	}
	
	return undef;
}

sub find_scalar_variable {
	my ($text) = @_;
	if ($text =~ /^($scalar_variable_qr)$/) {
		my $name = $1;
		$name =~ s/^\$//;
		return ({display_name => ('$'.$name), real_name => $name});
	} else {
		return;
	}
}

sub find_array_variable {
	my ($text) = @_;
	if ($text =~ /^($array_variable_qr)$/) {
		my $name = $1;
		$name =~ s/^\@//;
		return ({display_name => ('@'.$name), real_name => $name});
	} else {
		return;
	}
}

sub find_hash_variable {
	my ($text) = @_;
	if ($text =~ /^($hash_variable_qr)$/) {
		my $name = $1;
		$name =~ s/^\%//;
		return ({display_name => ('%'.$name), real_name => $name});
	} else {
		return;
	}
}

my %open_to_close_bracket_pair = ('[' => ']', '{' => '}');

sub find_accessed_variable {
	my ($text) = @_;
	if ($text =~ /^\$($valid_var_characters)(\[|\{)(.+)(\]|\})/) {
		my $name = $1;
		my $open_bracket = $2;
		my $complement = $3;
		return if (!defined $complement || $complement eq '');
		my $close_bracket = $4;
		
		my $type = ($open_bracket eq '[' ? 'accessed_array' : 'accessed_hash');
		my $close_bracket = (($type eq 'accessed_hash') ? '}' : ']');
		
		if ($open_to_close_bracket_pair{$open_bracket} ne $close_bracket) {
			return;
		}
		
		if ($type eq 'accessed_array') {
			return if ($complement !~ /^\d+$/ && !find_variable($complement));
			
		} elsif ($type eq 'accessed_hash') {
			return if ($complement !~ /^[a-zA-Z\d]+$/ && !find_variable($complement));
		}
		
		my $original_name = ('$' . $name . $open_bracket . $complement . $close_bracket);
		
		return {real_name => $name, type => $type, display_name => $original_name, complement => $complement};
	}
}

sub get_key_or_index {
	my ($open_char, $close_char, $code) = @_;
	my $counter = 0;
	my $key_index = '';
	my @characters = split('',$code);
	foreach my $current (@characters) {
		if ($current eq $open_char) {
			$counter++;
		} elsif ($current eq $close_char) {
			if ($counter == 0) {
				return $key_index;
			} else {
				$counter--;
			}
		}
		$key_index .= $current;
	}
	return undef;
}

1;
