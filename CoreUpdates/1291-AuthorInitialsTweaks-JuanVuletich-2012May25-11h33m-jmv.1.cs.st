'From Cuis 4.0 of 21 April 2012 [latest update: #1290] on 25 May 2012 at 11:40:36 am'!

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 5/25/2012 11:35'!
knownInitialsAndNames
	"This list could include people who hasn't contributed code to the Cuis image, but some optional package."
"
| all ok |
all _ Smalltalk allContributors asSet.
ok _ (Smalltalk contributorInitialsAndNames collect: [ :pair | pair first ]) asSet.
self assert: all = ok

initials         name"
^ #(
	#('ab' 					'Alexandre Bergel')
	#('abc' 					'Colin Putney')
	#('acg' 					'Andrew C. Greenberg')
	#('ads' 					'Adam Spitz')
	#('AFi' 					'Alain Fischer')
	#('ajh' 					'Anthony Hannan')
	#('al' 					'Adrian Lienhard')
	#('aoy' 					'Andres Otaduy')
	#('apb' 					'Andrew P. Black')
	#('ar' 					'Andreas Raab')
	#('asm' 				'Alejandro Magistrello')
	#('avi' 					'Avi Bryant')
	#('bf' 					'Bert Freudenberg')
	#('BG' 					'Boris Gaertner')
	#('BJP' 					'Bijan Parsia')
	#('bkv' 					'Brent Vukmer')
	#('bolot' 				'Bolot Kerimbaev')
	#('bp' 					'Bernhard Pieber')
	#('BP' 					'Brent Pinkney') 
	#('brp' 					'Brent Pinkney')
	#('cbc' 					'Chris Cunningham')
	#('cbr'					'Casey Ransberger')
	#('ccn' 					'Chris Norton')
	#('cmm' 				'Chris Muller')
	#('crl' 					'Craig Latta')
	#('cwp' 				'Colin Putney')
	#('das' 					'David A Smith')
	#('dc' 					'Damien Cassou')
	#('dew' 				'Doug Way')
	#('dgd' 				'Diego Gomez Deck')
	#('dhhi' 				'Dan Ingalls')
	#('di' 					'Dan Ingalls')
	#('djp' 					'David J. Pennell')
	#('DSM' 				'Duane Maxwell')
	#('DSG'					'David Graham')
	#('dtl' 					'Dave Lewis')
	#('dvf' 					'Daniel Vainsencher')
	#('eat' 					'Eric Arseneau Tremblay')
	#('eem'					'Eliot Emilio Miranda')
	#('efc' 					'Eddie Cottongim')
	#('em' 					'Ernest Micklei?')
	#('emm' 				'Ernest Micklei')
	#('fbs' 					'Frank Shearar')
	#('FBS' 					'Frank Shearar')
	#('fc' 					'Frank Caggiano')
	#('fcs' 					'Frank Sergeant')
	#('FernandoOlivero' 	'Fernando Olivero')
	#('FernanodOlivero' 	'Fernando Olivero')
	#('GabrielOmarCotelli' 	'Gabriel Omar Cotelli')
	#('gh' 					'Goran Krampe (nee Hultgren)')
	#('gk' 					'Goran Krampe (nee Hultgren)')
	#('gm' 					'German Morales')
	#('go' 					'Georg Gollmann')
	#('gsa' 					'German Arduino')
	#('hmm' 				'Hans-Martin Mosner')
	#('hsj' 					'Henrik Sperre Johansen')
	#('Igor.Stasenko' 		'Igor Stasenko')
	#('ikp' 					'Ian Piumarta')
	#('Jb' 					'Jean Baptiste Arnaud')
	#('jcg' 					'Joshua Gargus')
	#('jdr' 					'Javier Diaz-Reinoso')
	#('je' 					'Joern Eyrich')
	#('jf' 					'Julian Fitzell')
	#('JF' 					'Julian Fitzell')
	#('jhm' 					'John Maloney')
	#('jlb' 					'Jim Benson')
	#('jm' '					John Maloney')
	#('jmb' 					'Hans Baveco')
	#('JMG'					'Jeff Gonis')
	#('JMM' 				'John McIntosh')
	#('jmv' 					'Juan Vuletich')
	#('JMV' 					'Juan Vuletich')
	#('jp' 					'Joseph Pelrine')
	#('jrm' 					'John-Reed Maffeo')
	#('jrp' 					'John Pierce')
	#('jsp' 					'Jeff Pierce')
	#('kfr' 					'Karl Ramberg')
	#('KLC'			 		'Ken Causey')
	#('kph'					'Keith Hodges')
	#('KTT' 				'Kurt Thams')
	#('laza' 				'Alexander Lazarevic')
	#('LC' 					'Leandro Caniglia')
	#('len' 					'Luciano Esteban Notarfrancesco')
	#('lr' 					'Lukas Renggli')
	#('Lukas Renggli' 		'Lukas Renggli')
	#('ls' 					'Lex Spoon')
	#('md' 					'Marcus Denker')
	#('MarcusDenker' 		'Marcus Denker')
	#('marcus.denker' 		'Marcus Denker')
	#('mdr' 				'Mike Rutenberg')
	#('mga' 				'Markus Galli')
	#('mha' 				'Michael Haupt')
	#('mir' 					'Michael Rueger')
	#('mjg' 					'Mark Guzdial')
	#('mk' 					'Matej Kosik')
	#('MPH' 				'Michael Hewner')
	#('mpw' 				'Marcel Weiher')
	#('MPW' 				'Marcel Weiher')
	#('mrm' 				'Martin McClure')
	#('mtf' 					'Matthew Fulmer')
	#('mu' 					'Masashi Umezawa')
	#('nb' 					'Naala Brewer')
	#('nice'				 	'Nicolas Cellier')
	#('nk' 					'Ned Konz')
	#('nop' 					'Jay Carlson')
	#('NS' 					'Nathanael Schaerli')
	#('panda' 				'Michael Rueger')
	#('PHK' 				'Peter Keeler')
	#('Pmm' 				'Philippe Marschall')
	#('pnm' 				'Paul McDonough')
	#('r++' 				'Gerardo Richarte')
	#('raa' 					'Bob Arning')
	#('RAA' 					'Bob Arning')
	#('raok' 				'Richard A. O''Keefe')
	#('rca' 					'Russell Allen')
	#('reThink'			 	'Paul McDonough')
	#('rew' 					'Roger Whitney')
	#('rhi' 					'Robert Hirschfeld')
	#('rr' 					'Romain Robbes')
	#('rss' 					'Ron Spengler')
	#('rw' 					'Robert Withers')
	#('rww' 				'Robert Withers')
	#('Sames' 				'Samuel S. Shuster')
	#('sbw' 				'Stephan B. Wessels')
	#('sd' 					'Stephane Ducasse')
	#('SD' 					'Stephane Ducasse')
	#('sge' 					'Steve Elkins')
	#('sma' 				'Stefan Matthias Aust')
	#('sps' 					'Steven Swerling')
	#('SqR' 					'Andres Valloud')
	#('sr' 					'Stephan Rudlof')
	#('SSS' 				'Samuel S. Shuster')
	#('stephane.ducasse' 	'Stephane Ducasse')
	#('stephaneducasse' 	'Stephane Ducasse')
	#('stp' 					'Stephen Travis Pope')
	#('sumim' 				'Masato Sumi')
	#('svp' 					'Stephen Vincent Pair')
	#('sw' 					'Scott Wallace')
	#('TAG' 				'Travis Griggs')
	#('tak' 					'Takashi Yamamiya')
	#('tao' 					'Tim Olson')
	#('TBn' 					'Torsten Bergmann')
	#('tfei' 					'The Fourth Estate, Inc.')
	#('tfel' 					'Tim Felgentreff')
	#('th' 					'Torge Husfeldt')
	#('tk' 					'Ted Kaehler')
	#('tlk' 					'Tom Koenig')
	#('tpr' 					'Tim Rowledge')
	#('TPR' 					'Tim Rowledge')
	#('tween' 				'Andy Tween')
	#('ul' 					'Levente Uzonyi')
	#('vb' 					'Vassili Bykov')
	#('ward' 				'Ward Cunningham')
	#('wiz' 					'Jerome Peace')
	#('wod' 				'Bill Dargel')
	#('yo' 					'Yoshiki Ohshima')
	#('zz' 					'Serge Stinckwich'))! !


!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 5/25/2012 11:34'!
unknownContributors
	"Answer a collection of authorInitials for whom there is code in the system 
	(either in core or in loaded packages), but we don't knwo their full name.
	Smalltalk unknownContributors
	"

	| all ok |
	all _ Smalltalk allContributors asSet.
	ok _ (Smalltalk knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
	^(all difference: ok) asArray sort! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 5/25/2012 11:34'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorName |
	AuthorInitials _ (FillInTheBlank
		request: 'Please type your initials: '
		initialAnswer: AuthorInitials) withBlanksTrimmed.
	authorName _ (Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = AuthorInitials ]
		ifNone: [
			AuthorName _ (FillInTheBlank
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			^ self ]) second withBlanksTrimmed.
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [ AuthorName _ authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ]! !

!methodRemoval: SystemDictionary #contributorInitialsAndNames!
SystemDictionary removeSelector: #contributorInitialsAndNames!
