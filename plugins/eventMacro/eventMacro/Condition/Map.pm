package eventMacro::Condition::Map;

use strict;
use Globals qw( $field );
use base 'eventMacro::Condition::InMap';

sub _hooks {
	['packet_mapChange'];
}

sub _parse_syntax {
	my ( $self, $condition_code ) = @_;
	$self->{negate} = undef;
	if ($condition_code =~ /,/) {
		$self->{error} = "You can't use comma separated values on this Condition";
		return 0;
	} elsif ($condition_code =~ /^not/) {
        $self->{negate} = 1;
    }
	
	$self->SUPER::_parse_syntax( $condition_code );
}

sub validate_condition {
	my ( $self, $callback_type, $callback_name, $args ) = @_;
	
	if ($callback_type eq 'variable') {
		$self->update_validator_var($callback_name, $args);
	}
	
	$self->{lastMap} = $field->baseName;
	
	return $self->eventMacro::Condition::validate_condition( $self->validator_check_opposite($self->{lastMap}) );
}

sub get_new_variable_list {
	my ($self) = @_;
	my $new_variables;
	
	$new_variables->{".".$self->{name}."Last"} = $self->{lastMap};
	
	return $new_variables;
}