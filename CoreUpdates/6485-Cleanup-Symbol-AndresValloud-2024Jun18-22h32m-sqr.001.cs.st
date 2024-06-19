'From Cuis7.1 [latest update: #6452] on 18 June 2024 at 10:38:08 pm'!
!methodRemoval: WeakSet #do:after: stamp: 'sqr 6/18/2024 22:32:35'!
WeakSet removeSelector: #do:after:!
!methodRemoval: Symbol class #thatStarts:skipping: stamp: 'sqr 6/18/2024 22:32:07'!
Symbol class removeSelector: #thatStarts:skipping:!
!methodRemoval: Symbol class #allSymbolTablesDo:after: stamp: 'sqr 6/18/2024 22:32:20'!
Symbol class removeSelector: #allSymbolTablesDo:after:!
!methodRemoval: SymbolSet #do:after: stamp: 'sqr 6/18/2024 22:32:35'!
SymbolSet removeSelector: #do:after:!

!Symbol class reorganize!
('access' allSymbolTablesDo: allSymbols allSymbolsSelect: findInterned: selectorsContaining: selectorsMatching: thatStartsCaseSensitive:)
('class initialization' initialize)
('instance creation' fromCollectionOfStrings: intern: lookup: newFrom: readFrom:)
('private' basicFromString: rehash)
('services' hasInterned:and: hasInterned:ifTrue: hasInternedAndImplemented: hasInternedAndImplementedOrReferenced: possibleSelectorsFor:)
('testing' testSelectorPrefix)
!

