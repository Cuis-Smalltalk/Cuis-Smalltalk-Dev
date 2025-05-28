'From Cuis7.3 [latest update: #7249] on 28 May 2025 at 2:59:58 pm'!

!Number methodsFor: 'printing' stamp: 'jmv 9/3/2018 00:03:34'!
withDecimalUnitPrefixAndValue: aBlock
	"
	As in https://en.wikipedia.org/wiki/Metric_prefix
	{ 0.00000123456. 0.0000123456. 0.000123456. 0.00123456. 0.0123456. 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex := self log floor // 3.
	prefixIndex := prefixIndex min: 6 max: -6.
	factor := 1000 raisedToInteger: prefixIndex.
	nameAndSymbol := {
		{'atto'.		'a'}.
		{'femto'.	'f'}.
		{'pico'.		'p'}.
		{'nano'.		'n'}.
		{'micro'.		'µ'}.
		{'milli'.		'm'}.
		{''. 			''}.
		{'kilo'.		'k'}.
		{'mega'.		'M'}.
		{'giga'.		'G'}.
		{'tera'.		'T'}.
		{'peta'.		'P'}.
		{'exa'.		'E'}
	} at: prefixIndex+7.
	aBlock value: self asFloat / factor value: nameAndSymbol second value: nameAndSymbol first! !

