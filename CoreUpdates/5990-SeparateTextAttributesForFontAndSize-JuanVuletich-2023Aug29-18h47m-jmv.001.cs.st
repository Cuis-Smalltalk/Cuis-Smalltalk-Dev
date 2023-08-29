'From Cuis 6.0 [latest update: #5987] on 29 August 2023 at 7:11:41 pm'!
!classDefinition: #TextFontFamily category: 'System-Text-Attributes'!
TextAttribute subclass: #TextFontFamily
	instanceVariableNames: 'familyName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text-Attributes'!
!classDefinition: #TextFontSize category: 'System-Text-Attributes'!
TextAttribute subclass: #TextFontSize
	instanceVariableNames: 'pointSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text-Attributes'!

!TextAttribute methodsFor: 'iterating' stamp: 'jmv 8/29/2023 18:52:21'!
forFontFamilyDo: aBlock
	"No action is the default"! !

!TextAttribute methodsFor: 'iterating' stamp: 'jmv 8/29/2023 18:52:27'!
forFontSizeDo: aBlock
	"No action is the default"! !


!TextFontFamily methodsFor: 'accessing' stamp: 'jmv 8/29/2023 18:49:04'!
familyName
	^familyName! !

!TextFontFamily methodsFor: 'accessing' stamp: 'jmv 8/29/2023 18:58:38'!
familyName: aStringOrNil
	familyName := aStringOrNil.! !

!TextFontFamily methodsFor: 'comparing' stamp: 'jmv 8/29/2023 18:49:36'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [ other familyName = familyName ]! !

!TextFontFamily methodsFor: 'comparing' stamp: 'jmv 8/29/2023 18:49:04'!
dominates: other
	^ other class == self class! !

!TextFontFamily methodsFor: 'comparing' stamp: 'jmv 8/29/2023 18:49:58'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^familyName hash! !

!TextFontFamily methodsFor: 'iterating' stamp: 'jmv 8/29/2023 18:51:53'!
forFontFamilyDo: aBlock
	aBlock value: familyName! !

!TextFontFamily methodsFor: 'testing' stamp: 'jmv 8/29/2023 18:49:04'!
isFont
	^true! !


!TextFontFamily class methodsFor: 'instance creation' stamp: 'jmv 8/29/2023 18:58:29'!
familyName: aString
	"Reference only baseFonts. Any emphasis should be done with TextEmphasis."
	^ self new familyName: aString! !

!TextFontFamily class methodsFor: 'instance creation' stamp: 'jmv 8/29/2023 18:49:04'!
pointSize: aNumber
	"Reference only default family baseFont. Any emphasis should be done with TextEmphasis.
	Store only pointSize"
	^ self new familyName: nil pointSize: aNumber! !


!TextFontSize methodsFor: 'accessing' stamp: 'jmv 8/29/2023 18:49:17'!
pointSize
	^pointSize! !

!TextFontSize methodsFor: 'accessing' stamp: 'jmv 8/29/2023 18:58:46'!
pointSize: aNumber
	pointSize := aNumber! !

!TextFontSize methodsFor: 'comparing' stamp: 'jmv 8/29/2023 18:50:19'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [ other pointSize = pointSize ]! !

!TextFontSize methodsFor: 'comparing' stamp: 'jmv 8/29/2023 18:49:17'!
dominates: other
	^ other class == self class! !

!TextFontSize methodsFor: 'comparing' stamp: 'jmv 8/29/2023 18:50:30'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^pointSize hash! !

!TextFontSize methodsFor: 'iterating' stamp: 'jmv 8/29/2023 18:51:45'!
forFontSizeDo: aBlock
	aBlock value: pointSize! !

!TextFontSize methodsFor: 'testing' stamp: 'jmv 8/29/2023 18:49:17'!
isFont
	^true! !


!TextFontSize class methodsFor: 'instance creation' stamp: 'jmv 8/29/2023 18:59:02'!
pointSize: aNumber
	"Reference only baseFonts. Any emphasis should be done with TextEmphasis."
	^ self new pointSize: aNumber! !


!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:05:11'!
demoAncientEgyptian
	"
	UnicodeString demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 :=  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'NotoEgyptianHieroglyphs'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attributes: {TextFontFamily familyName: fontFamily familyName. TextFontSize pointSize: 60})
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:01:47'!
demoArabic
	"
	UnicodeString demoArabic
	"
	| s |
	s := 'وقد إذ حصدت تجهيز, قد بعد والإتحاد بالولايات. من مما أوروبا بولندا، والفلبين. هو المسرح بتطويق التحالف الا. بسبب أساسي المتاخمة عل يبق, حاول أمام الحكومة ان فصل. للجزر وتنامت شيء ثم. ضرب و ألمانيا التبرعات, كان كل خطّة تزامناً.'.

	(Text string: s attribute: (TextFontSize pointSize: 22)) editLabel: 'Arabic'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:02:16'!
demoArmenian
	"
	UnicodeString demoArmenian
	"
	| s |
	s :=  'լոռեմ իպսում դոլոռ սիթ ամեթ, սթեթ ինթելլեգաթ ծու մեի, պեռթինախ ոպոռթեռե աթ սեա. եոս ան մութաթ բլանդիթ. նեծ աթ նիսլ վոլումուս ապպելլանթուռ, ան հիս բռութե քուոդսի. սիթ ծեթեռոս ծոնթենթիոնես աթ, թե սումմո դիսծեռե պռո.'.

	(Text string: s attribute: (TextFontSize pointSize: 22)) 
		editLabel: 'Armenian'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:05:49'!
demoChinese
	"
	UnicodeString demoChinese
	"
	| utf8 fontFamily |
	utf8 :=  #[230 156 137 228 187 163 229 143 139 230 175 146 228 186 139 230 177 151 229 155 179 232 166 143 230 156 128 232 179 135 228 184 141 228 191 130 233 153 186 233 155 134 232 168 152 231 180 176 230 132 155 233 159 191 228 187 138 227 128 130 229 155 163 230 131 133 232 168 152 233 152 170 229 136 182 229 185 180 229 135 186 230 144 186 229 164 167 229 134 133 228 186 149 230 138 149 230 152 168 227 128 130 229 155 179 232 173 183 230 131 133 229 185 179 229 182 139 230 155 180 229 146 140 231 155 174 229 139 157 228 188 129 231 167 129 229 188 181 230 173 162 232 131 189 227 128 130 229 140 150 230 141 156 232 184 143 228 191 157 232 145 151 230 156 159 229 163 176 230 151 165 232 138 184 229 173 166 229 156 176 228 187 178 233 161 158 230 151 165 231 168 189 230 150 135 228 186 186 227 128 130 230 180 187 230 131 145 230 159 187 230 162 168 229 155 179 232 166 167 229 149 143 228 184 142 230 137 147 231 168 174 229 160 130 233 171 152 233 150 139 231 167 128 230 173 162 231 180 153 232 166 154 227 128 130 229 155 178 232 169 177 228 189 143 230 179 163 230 154 174 230 132 155 229 144 190 229 139 157 233 164 168 229 143 165 230 152 168 232 152 135 229 174 159 227 128 130 233 150 139 230 142 178 230 181 170 229 160 177 233 128 177 229 187 131 230 150 135 229 173 166 233 128 154 230 157 165 229 133 136 230 160 170 230 156 128 229 189 147 230 173 162 228 188 154 232 178 171 230 136 166 231 190 169 227 128 130 232 168 128 230 177 154 230 156 172 230 166 130 231 166 129 230 156 171 229 160 128 230 148 191 231 154 132 229 164 167 231 148 179 229 164 154 230 159 187 232 151 164 227 128 130 233 133 184 230 132 143 229 177 177 230 159 187 230 150 173 232 191 171 229 155 179 230 163 132 231 139 172 232 165 191 233 155 187 230 150 135 229 174 182 228 184 128 229 155 158 229 183 165 229 144 140 227 128 130].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'KiwiMaru'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attributes: {TextFontFamily familyName: fontFamily familyName. TextFontSize pointSize: 24})
		editLabel: 'Chinese'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:02:27'!
demoCyrillic
	"
	UnicodeString demoCyrillic
	"
	| s |
	s := 'Съешь ещё этих мягких французских булок, да выпей же чаю.
В чащах юга жил бы цитрус? да но фальшивый экземпляръ!!
Љубазни фењерџија чађавог лица хоће да ми покаже штос.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Cyrillic'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:03:23'!
demoGeorgian
	"
	UnicodeString demoGeorgian
	"
	| s |
	s := 'ლორემ იფსუმ დოლორ სით ამეთ, ვის ეა ფრიმის ასსენთიორ სცრიფთორემ, ეუმ ვერითუს ინიმიცუს ან. ნონუმy ვირთუთე ეხ ჰას, ად ბონორუმ სუსციფით ცონსთითუამ ვის. ათ ვის უთამურ იისყუე დისფუთათიონი, დუის ვენიამ ვიმ ეი, იდ ნეც ფეუგაით სენსერით ელეცთრამ. სით ნო ბრუთე რეცუსაბო თემფორიბუს, უთ ნეც დიცთა ოფფიციის ელოყუენთიამ.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Georgian'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:03:34'!
demoGreek
	"
	UnicodeString demoGreek
	"
	| s |
	s := 'Λορεμ ιπσθμ δολορ σιτ αμετ, νε πρι ρεγιονε cοπιοσαε σινγθλισ, αν εοσ αλια απειριαν ιραcθνδια. Φεθγαιτ διγνισσιμ μελ αν, ιν δθισ ιλλθδ λιβερ qθι. Ινερμισ μενανδρι λιβεραvισσε ναμ ετ. Ιδ νιβη σολετ αδιπισcινγ εστ. Cθ αδηθc δομινγ πρι. Εστ ιγνοτα τριτανι εα, μελ cηορο λαβιτθρ ιδ.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Greek'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:03:40'!
demoHebrew
	"
	UnicodeString demoHebrew
	"
	| s |
	s := 'עזה מדעי כלכלה ממונרכיה או, לערך מדינות האנציקלופדיה או לוח. היא לציין צרפתית לרפובליקה בה. מלא אחרים העברית בה, בשפה שימושיים של כתב. בקר שאלות זכויות מה. מדעי גיאוגרפיה אם צעד. באגים לרפובליקה שכל ב. שתי ב והוא העזרה תקשורת, דת ארץ ישראל אחרונים המקושרים.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Hebrew'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 8/29/2023 19:03:49'!
demoLatin
	"
	UnicodeString demoLatin
	"
	| s |
	s := 'Lorem ipsum dolor sit amet, populo latine aliquid pro no, sed te graeco percipit adipiscing. Definiebas dissentias mea in, vel nulla tollit ex. Aliquip persequeris eloquentiam his et. Cu diam novum animal vix, nam et probatus tractatos deterruisset. Percipit efficiantur pri ex, id animal debitis vulputate eos, ad ius posse error populo.'.

	(Text string: s attribute: (TextFontSize pointSize: 22))
		editLabel: 'Latin Alphabet'! !


!Text methodsFor: 'emphasis' stamp: 'jmv 8/29/2023 19:07:25'!
font: aFont
	"Apply aFont to the entire contents.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	self addAttribute: (TextFontFamily
			familyName: aFont familyName).
	self addAttribute: (TextFontSize
			pointSize: aFont pointSize).! !

!Text methodsFor: 'emphasis' stamp: 'jmv 8/29/2023 19:09:03'!
initialFont: aFont
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttributes |
	fontAttributes := {TextFontFamily			familyName: aFont familyName.
			TextFontSize pointSize: aFont pointSize}.
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes, fontAttributes ]]! !

!Text methodsFor: 'emphasis' stamp: 'jmv 8/29/2023 18:53:50'!
withAttributeValues: attributes do: aBlock
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor |
	paragraphStyle := nil.
	characterStyle := nil.
	familyName := nil.
	pointSize := nil.
	emphasis := 0.
	alignment := nil.
	color := nil.
	backgroundColor := nil.
	
	"ParagraphStyle is the first to set several values"
	attributes do: [ :attribute |
		attribute forParagraphStyleReferenceDo: [ :s |
			paragraphStyle := s.
			familyName := s familyName.
			pointSize := s pointSize.
			emphasis := s emphasis.
			alignment := s alignment.
			s color ifNotNil: [ :c | color := c ]]].

	"CharacterStyle, if present, can override font and color"
	attributes do: [ :attribute |
		attribute forCharacterStyleReferenceDo: [ :s |
			characterStyle := s.
			familyName := s familyName.
			pointSize := s pointSize.
			emphasis := s emphasis.
			s color ifNotNil: [ :c | color := c ]]].

	"These will not interfere with each other, and all of them take precedence over previous values"
	attributes do: [ :attribute |
		attribute forFontFamilyDo: [ :fn | familyName := fn ].
		attribute forFontSizeDo: [ :ps | pointSize := ps ].
		attribute forTextEmphasisDo: [ :e | emphasis := emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color := c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor := c ].
		attribute forTextAlignmentDo: [ :a | alignment := a ].
	].
	
	"Done. Now evaluate the block."
	^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor }! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 8/29/2023 19:03:58'!
pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	self addAttribute: (TextFontSize pointSize: pointSize)! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 8/29/2023 19:09:41'!
initialFont: aFont string: aString attribute: aTextAttribute
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	^self
		string: aString
		attributes: {
			TextFontFamily familyName: aFont familyName.
			TextFontSize pointSize: aFont pointSize.
			aTextAttribute}! !

!Text class methodsFor: 'instance creation' stamp: 'jmv 8/29/2023 19:10:05'!
initialFont: aFont stringOrText: aStringOrText
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	(aStringOrText is: #Text) ifTrue: [
		^aStringOrText initialFont: aFont ].
	
	^self string: aStringOrText attributes: {
			TextFontFamily familyName: aFont familyName.
			TextFontSize pointSize: aFont pointSize }! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 8/29/2023 19:10:48'!
offerFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| curFont newFont attr startIndex |
	startIndex := self startIndex.
	curFont := model actualContents fontAt: startIndex default: defaultFont.
	newFont := AbstractFont fromUser: curFont.
	newFont ifNil: [ ^self ].
	attr := TextFontFamily familyName: newFont familyName.
	self applyAttribute: attr.
	morph updateFromTextComposition.! !

!methodRemoval: TextFontSize class #familyName:pointSize: stamp: 'jmv 8/29/2023 18:59:39'!
TextFontSize class removeSelector: #familyName:pointSize:!
!methodRemoval: TextFontSize #familyName stamp: 'jmv 8/29/2023 18:50:10'!
TextFontSize removeSelector: #familyName!
!methodRemoval: TextFontSize #forFontFamilyAndSizeDo: stamp: 'jmv 8/29/2023 18:53:58'!
TextFontSize removeSelector: #forFontFamilyAndSizeDo:!
!methodRemoval: TextFontSize #familyName:pointSize: stamp: 'jmv 8/29/2023 18:59:38'!
TextFontSize removeSelector: #familyName:pointSize:!
!methodRemoval: TextFontSize #forBaseFontDo: stamp: 'jmv 8/29/2023 18:55:30'!
TextFontSize removeSelector: #forBaseFontDo:!
!methodRemoval: TextFontSize #font stamp: 'jmv 8/29/2023 18:57:01'!
TextFontSize removeSelector: #font!
!methodRemoval: TextFontFamily class #familyName:pointSize: stamp: 'jmv 8/29/2023 18:59:36'!
TextFontFamily class removeSelector: #familyName:pointSize:!
!methodRemoval: TextFontFamily #pointSize stamp: 'jmv 8/29/2023 18:49:28'!
TextFontFamily removeSelector: #pointSize!
!methodRemoval: TextFontFamily #forFontFamilyAndSizeDo: stamp: 'jmv 8/29/2023 18:54:00'!
TextFontFamily removeSelector: #forFontFamilyAndSizeDo:!
!methodRemoval: TextFontFamily #familyName:pointSize: stamp: 'jmv 8/29/2023 18:59:34'!
TextFontFamily removeSelector: #familyName:pointSize:!
!methodRemoval: TextFontFamily #forBaseFontDo: stamp: 'jmv 8/29/2023 18:55:31'!
TextFontFamily removeSelector: #forBaseFontDo:!
!methodRemoval: TextFontFamily #font stamp: 'jmv 8/29/2023 18:56:59'!
TextFontFamily removeSelector: #font!
!methodRemoval: TextAttribute #forFontFamilyAndSizeDo: stamp: 'jmv 8/29/2023 18:54:01'!
TextAttribute removeSelector: #forFontFamilyAndSizeDo:!
!methodRemoval: TextAttribute #forBaseFontDo: stamp: 'jmv 8/29/2023 18:55:31'!
TextAttribute removeSelector: #forBaseFontDo:!
!classRemoval: #TextFontFamilyAndSize stamp: 'jmv 8/29/2023 19:11:04'!
Smalltalk removeClassNamed: #TextFontFamilyAndSize!

