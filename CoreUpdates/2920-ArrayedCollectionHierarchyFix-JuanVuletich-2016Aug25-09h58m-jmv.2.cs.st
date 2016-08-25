'From Cuis 4.2 of 25 July 2013 [latest update: #2919] on 25 August 2016 at 10:30:44 am'!

!ArrayedCollection commentStamp: '<historical>' prior: 0!
I am an abstract superclass for collections of elements with the same storage size. My subclasses typically allocate storage as fixed size blocks. 

Element access is by an integer index, from 1 to collection size. Usual accessors are #at: and #at:put:

They are declared as having indexable instances. See:
	#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:

This means that they don't have instance variables. They are what is usually called an Array: a contiguous area of memory, that stores ObjectPointers (i.e. references to any objects), Bytes or 32-bit Words. Each class is free to use that storarge to encode other kinds of data. See, for example, FloatArray Float64Array or ShortIntegerArray. Therefore the size in bytes of each element is not constrained, although it is fixed for each class.

Instance size is fixed at creation. They can not grow or shrink.

For these reasons, there are many classes in the system that have Array like acessors (#at: and #at:put: with integer indexes) but are not in the ArrayedCollection hierarchy. These include OrderedCollection, Interval, Text, etc.!

!classDefinition: #RunArray category: #'Collections-Arrayed'!
SequenceableCollection subclass: #RunArray
	instanceVariableNames: 'runs values lastIndex lastRun lastOffset canJoinMessage '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #Text category: #'System-Text'!
SequenceableCollection subclass: #Text
	instanceVariableNames: 'string runs '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!RunArray methodsFor: 'adding' stamp: 'jmv 8/25/2016 10:20:31'!
add: newObject 
	"Adding to an Interval is not allowed."

	self shouldNotImplement! !


!Text methodsFor: 'adding' stamp: 'jmv 8/25/2016 10:21:27'!
add: newObject 
	"Adding to an Interval is not allowed."

	self shouldNotImplement! !

!classDefinition: #RunArray category: #'Collections-Arrayed'!
SequenceableCollection subclass: #RunArray
	instanceVariableNames: 'runs values lastIndex lastRun lastOffset canJoinMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #Text category: #'System-Text'!
SequenceableCollection subclass: #Text
	instanceVariableNames: 'string runs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Text reorganize!
('accessing' append: at: at:put: commandForReplaceFrom:to:with: customizeExplorerContents embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash)
('copying' copyFrom:to: copyReplaceFrom:to:with: copyReplaceTokens:with: postCopy)
('converting' asNumber asString asText displayStringOrText optimizedForMutationSpeed replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed withCuisLineEndings)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt:default: fontIfApplying:default: initialFont: initialFontFrom: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: removeAttributesThat: runLengthFor: withAttributeValues:do:)
('testing' is: isRemote)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph: paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic struck sub super under)
('*JSON-printing' jsonWriteOn:)
('adding' add:)
!

