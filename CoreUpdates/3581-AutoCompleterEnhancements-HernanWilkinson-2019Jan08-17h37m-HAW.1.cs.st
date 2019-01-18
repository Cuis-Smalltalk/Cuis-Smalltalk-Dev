'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 11 January 2019 at 12:22:25 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/9/2019 19:00:12'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClassOrEntries |

	id _ allSource copyFrom: range start to: range end.
	receiverClassOrEntries _ range rangeType 
		caseOf: {
			[ #globalVar ] 		-> [ (Smalltalk at: id asSymbol) class ].
			[ #self ] 				-> [ contextClass ].
			[ #super ] 				-> [ contextClass superclass ].
			[ #true ] 				-> [ True ].
			[ #false ] 				-> [ False ].
			[ #nil ] 				-> [ UndefinedObject ].
			[ #character ] 		-> [ id first class ].
			[ #number ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #string ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #symbol ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #stringSymbol ]  	-> [ self classOfLiteral: id in: contextClass ].
			[ #instVar ] 			-> [ specificModel classOfInstVarNamed: id ].
			[ #methodArg ] 		-> [ specificModel classOfTempVarNamed: id ].
			[ #tempVar ] 			-> [ specificModel classOfTempVarNamed: id ].
			[ #workspaceVar ]		-> [ specificModel classOfWorkspaceVarNamed: id ].
			[ #blockArg ]			-> [ specificModel classOfBlockArgNamed: id ].
			[ #blockTempVar ]		-> [ specificModel classOfBlockTempVarNamed: id ].
			[ #thisContext ] 		-> [ specificModel classOfThisContext ]. 
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #blockEnd ]			-> [ BlockClosure ].
			[ #arrayEnd ]			-> [ Array ].
			[ #rightBrace ]		-> [ Array ].
			[ #unary ]				-> [ self returnTypeOfUnaryMessage: id at: range ].
			[ #rightParenthesis ]  -> [ self returnTypeOfEnclosedExpressionAt: range ]. }
		otherwise: [ nil ].
	
	receiverClassOrEntries isCollection 
		ifTrue: [ self computeMessageEntriesForSelectors: receiverClassOrEntries ]
		ifFalse: [ 
"			Descomentar para que suceda solo para cuando el receiver es una clase"
			"(receiverClassOrEntries notNil and: [ receiverClassOrEntries isMeta ])"
			(receiverClassOrEntries notNil)
				ifTrue: [  self computeMessageForMetaclass: receiverClassOrEntries ] 
				ifFalse: [ self computeMessageEntries: receiverClassOrEntries ]]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/11/2019 12:17:09'!
computeMessageForMetaclass: aMetaclass 
	
	| selectorsToShow addedSelectorsFastSet current |
	
	true ifTrue: [ ^self computeMessageForMetaclassWithCategories: aMetaclass ].
	false ifTrue: [ ^self computeMessageForMetaclassAddingCategory: aMetaclass ].
	
	selectorsToShow := OrderedCollection new.
	addedSelectorsFastSet := IdentitySet new.
	current := aMetaclass.
	[ current notNil ] whileTrue: [ | currentSelectors |
		currentSelectors := current selectors select: [ :aSelector | (aSelector beginsWith: prefix) and: [ (addedSelectorsFastSet includes: aSelector) not ]].
		
		selectorsToShow addAll: currentSelectors sorted.
		addedSelectorsFastSet addAll: currentSelectors.
		
		current := current superclass.
		"Comentar/Descomentar abajo para poner separador por clase"
		"selectorsToShow add: '-- ', current name, ':'"].
	
	"Comentar/Descomentar abajo para poner separador con Object class"
	"selectorsToShow ifNotEmpty: [ selectorsToShow add: '-- ', current name ]."
	
	"self computeMessageEntries: Object class."
	"Falta sacar repetidos de entries"
	"entries := selectorsToShow asArray, entries."
	entries := selectorsToShow asArray.	

	"Comentar todo lo de arriba y descomentar esto para ver el comportamiento original"
	"
	self computeMessageEntries: aMetaclass .
	"
	   ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/9/2019 19:01:26'!
computeMessageForMetaclassAddingCategory: aMetaclass 
	
	| selectorsToShow addedSelectorsFastSet current |
	
	selectorsToShow := OrderedCollection new.
	addedSelectorsFastSet := IdentitySet new.
	current := aMetaclass.
	[ current notNil ] whileTrue: [ | currentSelectors organization |
		organization := current organization.
		currentSelectors := (current selectors select: [ :aSelector | (aSelector beginsWith: prefix) and: [ (addedSelectorsFastSet includes: aSelector) not ]]) sorted.
		currentSelectors do: [ :aSelector | selectorsToShow add: aSelector, ' | ', (organization categoryOfElement: aSelector) ].
		
		addedSelectorsFastSet addAll: currentSelectors.
		
		current := current superclass.
		"Comentar/Descomentar abajo para poner separador por clase"
		"selectorsToShow add: '-- ', current name, ':'"].
	
	"Comentar/Descomentar abajo para poner separador con Object class"
	selectorsToShow ifNotEmpty: [ selectorsToShow add: '-- ', current name ].
	
	self computeMessageEntries: Object class.
	"Falta sacar repetidos de entries"
	entries := selectorsToShow asArray, entries.
	
	"Comentar todo lo de arriba y descomentar esto para ver el comportamiento original"
	"
	self computeMessageEntries: aMetaclass .
	"
	   ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/9/2019 19:01:35'!
computeMessageForMetaclassWithCategories: aMetaclass 
	
	| selectorsToShow categoriesWithSelectors addedSelectorsFastSet current |

	categoriesWithSelectors := OrderedDictionary new.
	addedSelectorsFastSet := IdentitySet new.
	current := aMetaclass.
	[ current notNil ] whileTrue: [ | currentSelectors organization |
		organization := current organization.
		organization categories do: [ :aCategory | 
			currentSelectors := (organization listAtCategoryNamed: aCategory) 
				select: [ :aSelector |  (aSelector beginsWith: prefix) and: [ (addedSelectorsFastSet includes: aSelector) not ] ].
			(categoriesWithSelectors at: aCategory ifAbsentPut: [ OrderedCollection new ]) addAll: currentSelectors.
			addedSelectorsFastSet addAll: currentSelectors ].
		
		current := current superclass].
	 
	selectorsToShow := OrderedCollection new.
	categoriesWithSelectors 
		at: 'instance creation' 
		ifPresent: [ :instanceCreationSelectors | 
			instanceCreationSelectors isEmpty ifFalse: [
				selectorsToShow add: '-- instance creation'; addAll: instanceCreationSelectors.
				categoriesWithSelectors removeKey: 'instance creation' ]].
		  	
	categoriesWithSelectors associationsDo: [ :categoryAndSelectors |
		categoryAndSelectors value isEmpty ifFalse: [
		selectorsToShow 
			add: '-- ', categoryAndSelectors key;
			addAll: categoryAndSelectors value ]].
		
	"Comentar/Descomentar abajo para poner separador con Object class"
	"selectorsToShow ifNotEmpty: [ selectorsToShow add: '-- ', current name ]."
	
	"Falta sacar repetidos de entries"
"	self computeMessageEntries: Object class.
	entries := selectorsToShow asArray, entries."
	entries := selectorsToShow asArray.
	
	"Comentar todo lo de arriba y descomentar esto para ver el comportamiento original"
	"
	self computeMessageEntries: aMetaclass .
	"
	   ! !

