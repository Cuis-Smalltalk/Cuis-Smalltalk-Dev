'From Cuis 5.0 [latest update: #4228] on 19 June 2020 at 1:29:46 pm'!

!String methodsFor: 'grammar' stamp: 'LC 6/19/2020 12:52:49'!
exceptionalPlural
	| singular plural index |
	singular := #(
		'addendum' 'aircraft' 'alga' 'alumna' 'alumnus' 'amoeba' 'antenna' 'appendix' 'automaton'
		'bacillus' 'bacterium' 'barracks'
		'cactus' 'calculus' 'child' 'cicatrix' 'colossus' 'corpus' 'corrigendum' 'criterion' 'curriculum'
		'datum' 'deer' 'desideratum' 'dwarf'
		'echo' 'embargo' 'ephemeris' 'erratum' 'extremum'
		'fish' 'focus' 'foot' 'forum' 'fungus'
		'gallows' 'genus' 'goose'
		'hero'
		'index' 'infimum' 'is'
		'lacuna' 'larva' 'louse'
		'matrix' 'maximum' 'means' 'memorandum' 'minimum' 'mythos' 'money' 'mouse'
		'nucleus'
		'offspring' 'optimum' 'opus' 'ox'
		'person' 'phenomenon' 'phylum' 'potato' 'proof'
		'quantum'
		'roof'
		'series' 'sheep' 'species' 'spoof' 'stimulus' 'stratum' 'syllabus'
		'tomato' 'tooth' 'torpedo' 'trilby'
		'vertebra' 'vertex' 'veto'
		'was').
	plural := #(
		'addenda' 'aircraft' 'algae' 'alumnae' 'alumni' 'amoebae' 'antennae' 'appendices' 'automata'
		'bacilli' 'bacteria' 'barracks'
		'cacti' 'calculi' 'children' 'cicatrices' 'colossi' 'corpora' 'corrigenda' 'criteria' 'curricula'
		'data' 'deer' 'desiderata' 'dwarfs'
		'echoes' 'embargoes' 'ephemerides' 'errata' 'extrema'
		'fish' 'foci' 'feet' 'fora' 'fungi'
		'gallows' 'genera' 'geese'
		'heroes'
		'indices' 'infima' 'are'
		'lacunae' 'larvae' 'lice'
		'matrices' 'maxima' 'means' 'memoranda' 'minima' 'mythoi' 'moneys' 'mice'
		'nuclei'
		'offspring' 'optima' 'opera' 'oxen'
		'people' 'phenomena' 'phyla' 'potatoes' 'proofs'
		'quanta'
		'roofs'
		'series' 'sheep' 'species' 'spoofs' 'stimuli' 'strata' 'syllabi'
		'tomatoes' 'teeth' 'torpedoes' 'trilbys'
		'vertebrae' 'vertices' 'vetoes'
		'were').
	index := singular indexOf: self.
	^index > 0 ifTrue: [plural at: index]! !

