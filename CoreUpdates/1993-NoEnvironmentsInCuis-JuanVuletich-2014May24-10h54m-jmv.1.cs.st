'From Cuis 4.2 of 25 July 2013 [latest update: #1992] on 24 May 2014 at 11:36:47.780456 am'!

!Metaclass commentStamp: '<historical>' prior: 0!
My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
[Subtle] In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
	Integer superclass == Number, and
	Integer class superclass == Number class.
However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
	Object superclass == nil, and
	Object class superclass == Class.
!


!SHParserST80 commentStamp: '<historical>' prior: 0!
I am a Smalltalk method / expression parser.

Rather than creating an Abstract Syntax Tree, I create a sequence of SHRanges (in my 'ranges' instance variable), which represent the tokens within the String I am parsing.

I am used by a SHTextStylerST80 to parse method source strings.
I am able to parse incomplete / incorrect methods, and so can be used to parse methods that are being edited.

My 'source' instance variable should be set to the string to be parsed.

My 'classOrMetaClass' instance var must be set to the class or metaClass for the method source so that I can correctly resolve identifiers within the source. If this is nil , I parse the source as an expression (i.e. a doIt expression).

My 'workspace' instance variable can be set to a Workspace, so that I can resolve workspace variables.

Example 1.
	ranges := SHParserST80 new
		classOrMetaClass: Object;
		source: 'testMethod ^self';
		parse;
		ranges
!


!Browser methodsFor: 'class functions' stamp: 'jmv 5/24/2014 11:07'!
defineClass: defString notifying: aController  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aController."
	| oldClass class newClassName defTokens keywdIx |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx _ defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newClassName , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Compiler
				evaluate: defString
				notifying: aController
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryList.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !

!Browser methodsFor: 'class list' stamp: 'jmv 5/24/2014 10:56'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	| name |
	(name _ self selectedClassName) ifNil: [^ nil].
	^ Smalltalk at: name ifAbsent: nil! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/24/2014 11:21'!
definition
	"Answer a String that defines the receiver."

	^String streamContents: [ :strm |
		strm
			nextPutAll: (superclass ifNotNil: [ superclass name ] ifNil: [ 'ProtoObject' ]);
			nextPutAll: self kindOfSubclass;
			store: self name.
		strm
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString.
		strm
			newLine;
			tab;
			nextPutAll: 'category: ';
			store: (SystemOrganization categoryOfElement: self name) asString.

		superclass ifNil: [ 
			strm nextPutAll: '.'; newLine.
			strm nextPutAll: self name.
			strm space; nextPutAll: 'superclass: nil' ]]! !


!Class methodsFor: 'compiling' stamp: 'jmv 5/24/2014 11:00'!
bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol |
	aSymbol _ varName asSymbol.

	"First look in receiver, and up the hierarchy. Inherited variables take precedence over globals."
	(self localBindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in globals."
	(Smalltalk bindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Fail at the end."
	^nil! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 5/24/2014 10:59'!
changeFromCategorySpecs: categorySpecs 
	"Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane."

	| oldElements newElements newCategories newStops currentStop temp cc catSpec |
	oldElements _ elementArray asSet.
	newCategories _ Array new: categorySpecs size.
	newStops _ Array new: categorySpecs size.
	currentStop _ 0.
	newElements _ WriteStream on: (Array new: 16).
	1 to: categorySpecs size do: [ :i | 
		catSpec _ categorySpecs at: i.
		newCategories at: i put: catSpec first asSymbol.
		catSpec allButFirst asArray sort do:
			[:elem |
			(oldElements remove: elem ifAbsent: nil) ifNotNil: [
				newElements nextPut: elem.
				currentStop _ currentStop+1]].
		newStops at: i put: currentStop].

	"Ignore extra elements but don't lose any existing elements!!"
	oldElements _ oldElements collect: [ :elem | Array with: (self categoryOfElement: elem) with: elem].
	newElements _ newElements contents.
	categoryArray _ newCategories.
	(cc _ categoryArray asSet) size = categoryArray size ifFalse: [ "has duplicate element"
		temp _ categoryArray asOrderedCollection.
		temp removeAll: categoryArray asSet asOrderedCollection.
		temp do: [ :each | | dup ii |
			dup _ each.
			ii _ categoryArray indexOf: dup.
			[ dup _ (dup,' #2') asSymbol.  cc includes: dup ] whileTrue.
			cc add: dup.
			categoryArray at: ii put: dup]].
	categoryStops _ newStops.
	elementArray _ newElements.
	oldElements do: [:pair | self classify: pair last under: pair first]! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/24/2014 10:54'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category
	"Define a new class"
	^self 
		name: className 
		subclassOf: newSuper 
		type: type 
		instanceVariableNames: instVarString 
		classVariableNames: classVarString 
		poolDictionaries: poolString 
		category: category
		unsafe: false! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 5/24/2014 10:57'!
superclass: aClass
	variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	| oldClassOrNil actualType |
	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a byte subclass of a class with named fields'].
	(aClass isVariable and: [aClass isWords])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with word fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with pointer fields'].
	oldClassOrNil := Smalltalk at: t ifAbsent: nil.
	actualType := (oldClassOrNil notNil
				   and: [oldClassOrNil typeOfClass == #compiledMethod])
					ifTrue: [#compiledMethod]
					ifFalse: [#bytes].
	^self 
		name: t
		subclassOf: aClass
		type: actualType
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat! !


!SystemDictionary methodsFor: 'dictionary access' stamp: 'jmv 5/24/2014 11:05'!
associationOrUndeclaredAt: key 
	"return an association or install in undeclared. Used for mating up ImageSegments."

	^ self associationAt: key ifAbsent: [
		Undeclared at: key put: nil.
		Undeclared associationAt: key ]! !


!Encoder methodsFor: 'results' stamp: 'jmv 5/24/2014 10:57'!
associationForClass
	| assoc |
	assoc _ Smalltalk associationAt: class name ifAbsent: [ nil ].
	^assoc value == class
		ifTrue: [ assoc ]
		ifFalse: [ Association new value: class ]! !


!Parser methodsFor: 'error correction' stamp: 'jmv 5/24/2014 10:58'!
declareGlobal: name
	| sym |
	sym _ name asSymbol.
	^encoder
		global: (Smalltalk
			at: sym put: nil;
			associationAt: sym)
		name: sym! !


!SharedPool class methodsFor: 'name lookup' stamp: 'jmv 5/24/2014 11:03'!
bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol binding |
	aSymbol _ varName asSymbol.

	"First look in classVar dictionary."
	binding _ self classPool bindingOf: aSymbol.
	binding ifNotNil: [ ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		| poolBinding |
		poolBinding _ pool bindingOf: aSymbol.
		poolBinding ifNotNil: [ ^poolBinding ].
	].

	"subclassing is not preserved"
	^nil! !

!methodRemoval: UndefinedObject #environment!
UndefinedObject removeSelector: #environment!
!methodRemoval: Stream #isTypeHTTP!
Stream removeSelector: #isTypeHTTP!
!methodRemoval: Stream #nextMatchFor:!
Stream removeSelector: #nextMatchFor:!
!methodRemoval: Encoder #environment!
Encoder removeSelector: #environment!
!methodRemoval: BlockContext #fullPrintOn:!
BlockContext removeSelector: #fullPrintOn:!
!methodRemoval: BlockContext #myEnv!
BlockContext removeSelector: #myEnv!
!methodRemoval: SystemDictionary #associationAtOrAbove:ifAbsent:!
SystemDictionary removeSelector: #associationAtOrAbove:ifAbsent:!
!methodRemoval: SystemDictionary #atOrAbove:ifAbsent:!
SystemDictionary removeSelector: #atOrAbove:ifAbsent:!
!methodRemoval: SystemDictionary #environmentForCategory:!
SystemDictionary removeSelector: #environmentForCategory:!
!methodRemoval: SystemDictionary #includesKeyOrAbove:!
SystemDictionary removeSelector: #includesKeyOrAbove:!
!methodRemoval: SystemDictionary #scopeFor:from:envtAndPathIfFound:!
SystemDictionary removeSelector: #scopeFor:from:envtAndPathIfFound:!
!methodRemoval: ClassBuilder #name:inEnvironment:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category:!
ClassBuilder removeSelector: #name:inEnvironment:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category:!
!methodRemoval: BlockClosure #fullPrintOn:!
BlockClosure removeSelector: #fullPrintOn:!
!methodRemoval: Behavior #environment!
Behavior removeSelector: #environment!
!methodRemoval: Browser #selectedEnvironment!
Browser removeSelector: #selectedEnvironment!
