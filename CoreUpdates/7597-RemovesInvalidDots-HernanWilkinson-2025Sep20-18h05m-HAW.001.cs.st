'From Cuis7.5 [latest update: #7594] on 20 September 2025 at 6:16:01 pm'!

!FormGlyph methodsFor: 'initialization' stamp: 'HAW 9/20/2025 18:05:56'!
form: aForm leftOffsetBy: aNumber
	form := aForm.
	leftOffset := leftOffset + aNumber! !


!Bag methodsFor: 'comparing' stamp: 'HAW 9/20/2025 18:10:17'!
= aBag
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them"

	self == aBag ifTrue: [ ^ true ].		"Any object is equal to itself"
	(aBag is: #Bag) ifFalse: [ ^ false ].
	self size = aBag size ifFalse: [ ^ false ].
	contents associationsDo: [ :assoc |
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [ ^ false ]].
	^true! !


!Dictionary methodsFor: 'comparing' library: #MinimalKernel04HashedCollections stamp: 'HAW 9/20/2025 18:07:38'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value"

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	(aDictionary is: #Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !


!Message methodsFor: 'stub creation' stamp: 'HAW 9/20/2025 18:10:32'!
writeShouldBeImplementedOn: stream

	stream				
		nextPutAll: 'self ';
		nextPutAll: #shouldBeImplemented;
		nextPut: $.! !


!TextParagraphMorph methodsFor: 'accessing' stamp: 'HAW 9/20/2025 18:06:23'!
contents: aStringOrText

	| current |
	current := textComposition textComposed.
	current = aStringOrText ifTrue: [
		((current is: #Text) and: [aStringOrText is: #Text])
		ifTrue: [
			current runs = aStringOrText runs ifTrue: [
				^self ]]
		ifFalse: [
			^self ]].
	textComposition	 	setModel: (TextModel withText: aStringOrText).
	self reflowAndMaybeResize.! !


!VectorEngine class methodsFor: 'experiments' stamp: 'HAW 9/20/2025 18:07:03'!
experLinesAndShapes2
	"
	VectorEngine experLinesAndShapes2
	"
	| engine y1 dy y2 aaw |
	engine := self onForm: Display subPixelAntiAliasing: true.
	aaw := engine class defaultAntiAliasingWidth.

	y1 := 10.
	dy := 3.
	y2 := aaw*4*2.
	15 timesRepeat: [
		"stroke is narrow but visible"
		engine strokeWidth: 1.0 color: Color green fillColor: Color red do: [
			engine
				moveTo: 131@y1; lineTo: 230@(y1+dy);
				lineTo: 230@(y1+y2+dy); lineTo: 131@(y1+y2);
				lineToFirstPoint.
			engine
				moveTo: y1@131; lineTo: y1+dy@230;
				lineTo: y1+y2+dy@230; lineTo: y1+y2@131;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.8 color: Color green fillColor: Color red do: [
			engine
				moveTo: 231@(y1+dy); lineTo: 330@(y1+dy+dy);
				lineTo: 330@(y1+y2+dy+dy); lineTo: 231@(y1+y2+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy@231; lineTo: y1+dy+dy@330;
				lineTo: y1+y2+dy+dy@330; lineTo: y1+y2+dy@231;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.6 color: Color green fillColor: Color red do: [
			engine
				moveTo: 331@(y1+dy+dy); lineTo: 430@(y1+dy+dy+dy);
				lineTo: 430@(y1+y2+dy+dy+dy); lineTo: 331@(y1+y2+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy@331; lineTo: y1+dy+dy+dy@430;
				lineTo: y1+y2+dy+dy+dy@430; lineTo: y1+y2+dy+dy@331;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.4 color: Color green fillColor: Color red do: [
			engine
				moveTo: 431@(y1+dy+dy+dy); lineTo: 530@(y1+dy+dy+dy+dy);
				lineTo: 530@(y1+y2+dy+dy+dy+dy); lineTo: 431@(y1+y2+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy@431; lineTo: y1+dy+dy+dy+dy@530;
				lineTo: y1+y2+dy+dy+dy+dy@530; lineTo: y1+y2+dy+dy+dy@431;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.2 color: Color green fillColor: Color red do: [
			engine
				moveTo: 531@(y1+dy+dy+dy+dy); lineTo: 630@(y1+dy+dy+dy+dy+dy);
				lineTo: 630@(y1+y2+dy+dy+dy+dy+dy); lineTo: 531@(y1+y2+dy+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy+dy@531; lineTo: y1+dy+dy+dy+dy+dy@630;
				lineTo: y1+y2+dy+dy+dy+dy+dy@630; lineTo: y1+y2+dy+dy+dy+dy@531;
				lineToFirstPoint ].
		"stroke zero means no stroke"
		engine strokeWidth: 0.0 color: Color green fillColor: Color red do: [
			engine
				moveTo: 631@(y1+dy+dy+dy+dy+dy); lineTo: 730@(y1+dy+dy+dy+dy+dy+dy);
				lineTo: 730@(y1+y2+dy+dy+dy+dy+dy+dy); lineTo: 631@(y1+y2+dy+dy+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy+dy+dy@631; lineTo: y1+dy+dy+dy+dy+dy+dy@730;
				lineTo: y1+y2+dy+dy+dy+dy+dy+dy@730; lineTo: y1+y2+dy+dy+dy+dy+dy@631;
				lineToFirstPoint ].
		y1 := y1 + y2 + aaw + 3.
		Display forceToScreen.
		y2 print.
		y2 := y2 * 0.8.
	].
	
	Display forceToScreen! !

