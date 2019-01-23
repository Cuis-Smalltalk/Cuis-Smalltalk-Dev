'From Cuis 5.0 of 7 November 2016 [latest update: #3319] on 12 May 2018 at 5:44:44 pm'!
'Description NBS-ISCC color names originating from Munsell''s idea of a rational naming scheme based around "color centroids":
	http://tx4.us/nbs-iscc.htm 

See Class side:
	Color nbsisccColorDictionary explore.

To reset the Color name->color dictionary:
 	Color setColorNamesDict: (Color nbsisccColorDictionary).'!
!provides: 'NBSISCC-NamedColors' 1 1!
!requires: 'Graphics-Files-Additional' 1 nil nil!
!requires: 'Color-Extras' 1 4 nil!
!requires: 'Cuis-Base' 42 1788 nil!



!Color class methodsFor: '*nbsiscc-namedcolors' stamp: 'KenD 1/28/2013 19:59'!
nbsisccColorDictionary
	"Answer a dictionary of NBS-ISCC compliant color names to Color objects.
	http://tx4.us/nbs-iscc.htm"

"
	self nbsisccColorDictionary explore.

To reset the Color name->color dictionary:
 	Color setColorNamesDict: (Color nbsisccColorDictionary).
"
	| nbsisccDict |
	nbsisccDict _ self nbsisccColors1 addAll: 
		self nbsisccColors2; addAll: 
		self nbsisccColors3; yourself.
		
	^nbsisccDict! !

!Color class methodsFor: '*nbsiscc-namedcolors' stamp: 'KenD 1/28/2013 19:38'!
nbsisccColors1
	"Answer a dictionary of NBS-ISCC compliant color names to Color objects.
	First half!!"

	^Dictionary new
    at: #vividPink  put: (Color fromString: '#fFB5BA');
    at: #strongPink  put: (Color fromString: '#eA9399');
    at: #deepPink  put: (Color fromString: '#e4717A');
    at: #lightPink  put: (Color fromString: '#f9CCCA');
    at: #moderatePink  put: (Color fromString: '#dEA5A4');
    at: #darkPink  put: (Color fromString: '#c08081');
    at: #palePink  put: (Color fromString: '#eAD8D7');
    at: #grayishPink  put: (Color fromString: '#c4AEAD');
    at: #pinkishWhite  put: (Color fromString: '#eAE3E1');
    at: #pinkishGray  put: (Color fromString: '#c1B6B3');
    at: #vividRed  put: (Color fromString: '#bE0032');
    at: #strongRed  put: (Color fromString: '#bC3F4A');
    at: #deepRed  put: (Color fromString: '#841B2D');
    at: #veryDeepRed  put: (Color fromString: '#5C0923');
    at: #moderateRed  put: (Color fromString: '#aB4E52');
    at: #darkRed  put: (Color fromString: '#722F37');
    at: #veryDarkRed  put: (Color fromString: '#3F1728');
    at: #lightGrayishRed put: (Color fromString: '#aD8884');
    at: #grayishRed  put: (Color fromString: '#905D5D');
    at: #darkGrayishRed put: (Color fromString: '#543D3F');
    at: #blackishRed  put: (Color fromString: '#2E1D21');
    at: #reddishGray  put: (Color fromString: '#8F817F');
    at: #darkReddishGray put: (Color fromString: '#5C504F');
    at: #reddishBlack  put: (Color fromString: '#282022');
    at: #vividYellowishPink put: (Color fromString: '#fFB7A5');
    at: #strongYellowishPink put: (Color fromString: '#f99379');
    at: #deepYellowishPink put: (Color fromString: '#e66721');
    at: #lightYellowishPink put: (Color fromString: '#f4C2C2');
    at: #moderateYellowishPink put: (Color fromString: '#d9A6A9');
    at: #darkYellowishPink put: (Color fromString: '#c48379');
    at: #paleYellowishPink put: (Color fromString: '#eCD5C5');
    at: #grayishYellowishPink put: (Color fromString: '#c7ADA3');
    at: #brownishPink  put: (Color fromString: '#c2AC99');
    at: #vividReddishOrange put: (Color fromString: '#e25822');
    at: #strongReddishOrange put: (Color fromString: '#d9603B');
    at: #deepReddishOrange put: (Color fromString: '#aA381E');
    at: #moderateReddishOrange put: (Color fromString: '#cB6D51');
    at: #darkReddishOrange put: (Color fromString: '#9E4732');
    at: #grayishReddishOrange put: (Color fromString: '#b4745E');
    at: #strongReddishBrown put: (Color fromString: '#882D17');
    at: #deepReddishBrown put: (Color fromString: '#56070C');
    at: #lightReddishBrown put: (Color fromString: '#a87C6D');
    at: #moderateReddishBrown put: (Color fromString: '#79443B');
    at: #darkReddishBrown put: (Color fromString: '#3E1D1E');
    at: #lightGrayishReddishBrown  put: (Color fromString: '#977F73');
    at: #grayishReddishBrown put: (Color fromString: '#674C47');
    at: #darkGrayishReddishBrown  put: (Color fromString: '#43302E');
    at: #vividOrange  put: (Color fromString: '#f38400');
    at: #brilliantOrange put: (Color fromString: '#fD943F');
    at: #strongOrange  put: (Color fromString: '#eD872D');
    at: #deepOrange  put: (Color fromString: '#bE6516');
    at: #lightOrange  put: (Color fromString: '#fAB57F');
    at: #moderateOrange  put: (Color fromString: '#d99058');
    at: #brownishOrange  put: (Color fromString: '#aE6938');
    at: #strongBrown  put: (Color fromString: '#80461B');
    at: #deepBrown  put: (Color fromString: '#593319');
    at: #lightBrown  put: (Color fromString: '#a67B5B');
    at: #moderateBrown  put: (Color fromString: '#6F4E37');
    at: #darkBrown  put: (Color fromString: '#422518');
    at: #lightGrayishBrown put: (Color fromString: '#958070');
    at: #grayishBrown  put: (Color fromString: '#635147');
    at: #darkGrayishBrown put: (Color fromString: '#3E322C');
    at: #lightBrownishGray put: (Color fromString: '#8E8279');
    at: #brownishGray  put: (Color fromString: '#5B504F');
    at: #brownishBlack  put: (Color fromString: '#28201C');
    at: #vividOrangeYellow put: (Color fromString: '#f6A600');
    at: #brilliantOrangeYellow put: (Color fromString: '#fFC14F');
    at: #strongOrangeYellow put: (Color fromString: '#eAA221');
    at: #deepOrangeYellow put: (Color fromString: '#c98500');
    at: #lightOrangeYellow put: (Color fromString: '#fBC97F');
    at: #moderateOrangeYellow put: (Color fromString: '#e3A857');
    at: #darkOrangeYellow put: (Color fromString: '#bE8A3D');
    at: #paleOrangeYellow put: (Color fromString: '#fAD6A5');
    at: #strongYellowishBrown put: (Color fromString: '#996515');
    at: #deepYellowishBrown put: (Color fromString: '#654522');
    at: #lightYellowishBrown put: (Color fromString: '#c19A6B');
    at: #moderateYellowishBrown  put: (Color fromString: '#826644');
    at: #darkYellowishBrown put: (Color fromString: '#4B3621');
    at: #lightGrayishYellowishBrown  put: (Color fromString: '#aE9B82');
    at: #grayishYellowishBrown put: (Color fromString: '#7E6D5A');
    at: #darkGrayishYellowishBrown  put: (Color fromString: '#483C32');
    at: #vividYellow  put: (Color fromString: '#f3C300');
    at: #brilliantYellow put: (Color fromString: '#fADA5E');
    at: #strongYellow  put: (Color fromString: '#d4AF37');
    at: #deepYellow  put: (Color fromString: '#aF8D13');
    at: #lightYellow  put: (Color fromString: '#f8DE7E');
    at: #moderateYellow  put: (Color fromString: '#c9AE5D');
    at: #darkYellow  put: (Color fromString: '#aB9144');
    at: #paleYellow  put: (Color fromString: '#f3E5AB');
    at: #grayishYellow  put: (Color fromString: '#c2B280');
    at: #darkGrayishYellow put: (Color fromString: '#a18F60');
    at: #yellowishWhite  put: (Color fromString: '#f0EAD6');
    at: #yellowishGray  put: (Color fromString: '#bFB8A5');
    at: #lightOliveBrown put: (Color fromString: '#967117');
    at: #moderateOliveBrown put: (Color fromString: '#6C541E');
    at: #darkOliveBrown put: (Color fromString: '#3B3121');
    at: #vividGreenishYellow put: (Color fromString: '#dCD300');
    at: #brilliantGreenishYellow  put: (Color fromString: '#e9E450');
    at: #strongGreenishYellow put: (Color fromString: '#bEB72E');
    at: #deepGreenishYellow put: (Color fromString: '#9B9400');
    at: #lightGreenishYellow put: (Color fromString: '#eAE679');
    at: #moderateGreenishYellow  put: (Color fromString: '#b9B459');
    at: #darkGreenishYellow put: (Color fromString: '#98943E');
    at: #paleGreenishYellow put: (Color fromString: '#eBE8A4');
    at: #grayishGreenishYellow put: (Color fromString: '#b9B57D');
    at: #lightOlive  put: (Color fromString: '#867E36');
    at: #moderateOlive  put: (Color fromString: '#665D1E');
    at: #darkOlive  put: (Color fromString: '#403D21');
    at: #lightGrayishOlive put: (Color fromString: '#8C8767');
    at: #grayishOlive  put: (Color fromString: '#5B5842');
    at: #darkGrayishOlive put: (Color fromString: '#363527');
    at: #lightOliveGray put: (Color fromString: '#8A8776');
    at: #oliveGray  put: (Color fromString: '#57554C');
    at: #oliveBlack  put: (Color fromString: '#25241D');
		yourself! !

!Color class methodsFor: '*nbsiscc-namedcolors' stamp: 'KenD 1/28/2013 19:38'!
nbsisccColors2
	"Answer a dictionary of NBS-ISCC compliant color names to Color objects.
	First half!!"

	^Dictionary new
    at: #vividYellowGreen put: (Color fromString: '#8DB600');
    at: #brilliantYellowGreen put: (Color fromString: '#bDDA57');
    at: #strongYellowGreen put: (Color fromString: '#7E9F2E');
    at: #deepYellowGreen put: (Color fromString: '#467129');
    at: #lightYellowGreen put: (Color fromString: '#c9DC89');
    at: #moderateYellowGreen put: (Color fromString: '#8A9A5B');
    at: #paleYellowGreen put: (Color fromString: '#dADFB7');
    at: #grayishYellowGreen put: (Color fromString: '#8F9779');
    at: #strongOliveGreen put: (Color fromString: '#404F00');
    at: #deepOliveGreen put: (Color fromString: '#232F00');
    at: #moderateOliveGreen put: (Color fromString: '#4A5D23');
    at: #darkOliveGreen put: (Color fromString: '#2B3D26');
    at: #grayishOliveGreen put: (Color fromString: '#515744');
    at: #darkGrayishOliveGreen  put: (Color fromString: '#31362B');
    at: #vividYellowishGreen put: (Color fromString: '#27A64C');
    at: #brilliantYellowishGreen  put: (Color fromString: '#83D37D');
    at: #strongYellowishGreen put: (Color fromString: '#44944A');
    at: #deepYellowishGreen put: (Color fromString: '#00622D');
    at: #veryDeepYellowishGreen  put: (Color fromString: '#003118');
    at: #veryLightYellowishGreen  put: (Color fromString: '#b6E5AF');
    at: #lightYellowishGreen put: (Color fromString: '#93C592');
    at: #moderateYellowishGreen  put: (Color fromString: '#679267');
    at: #darkYellowishGreen put: (Color fromString: '#355E3B');
    at: #veryDarkYellowishGreen  put: (Color fromString: '#173620');
    at: #vividGreen  put: (Color fromString: '#008856');
    at: #brilliantGreen  put: (Color fromString: '#3EB489');
    at: #strongGreen  put: (Color fromString: '#007959');
    at: #deepGreen  put: (Color fromString: '#00543D');
    at: #veryLightGreen put: (Color fromString: '#8ED1B2');
    at: #lightGreen  put: (Color fromString: '#6AAB8E');
    at: #moderateGreen  put: (Color fromString: '#3B7861');
    at: #darkGreen  put: (Color fromString: '#1B4D3E');
    at: #veryDarkGreen  put: (Color fromString: '#1C352D');
    at: #veryPaleGreen  put: (Color fromString: '#c7E6D7');
    at: #paleGreen  put: (Color fromString: '#8DA399');
    at: #grayishGreen  put: (Color fromString: '#5E716A');
    at: #darkGrayishGreen put: (Color fromString: '#3A4B47');
    at: #blackishGreen  put: (Color fromString: '#1A2421');
    at: #greenishWhite  put: (Color fromString: '#dFEDE8');
    at: #lightGreenishGray put: (Color fromString: '#b2BEB5');
    at: #greenishGray  put: (Color fromString: '#7D8984');
    at: #darkGreenishGray put: (Color fromString: '#4E5755');
    at: #greenishBlack  put: (Color fromString: '#1E2321');
    at: #vividBluishGreen put: (Color fromString: '#008882');
    at: #brilliantBluishGreen put: (Color fromString: '#00A693');
    at: #strongBluishGreen put: (Color fromString: '#007A74');
    at: #deepBluishGreen put: (Color fromString: '#00443F');
    at: #veryLightBluishGreen put: (Color fromString: '#96DED1');
    at: #lightBluishGreen put: (Color fromString: '#66ADA4');
    at: #moderateBluishGreen put: (Color fromString: '#317873');
    at: #darkBluishGreen put: (Color fromString: '#004B49');
    at: #veryDarkBluishGreen put: (Color fromString: '#002A29');
    at: #vividGreenishBlue put: (Color fromString: '#0085A1');
    at: #brilliantGreenishBlue put: (Color fromString: '#239EBA');
    at: #strongGreenishBlue put: (Color fromString: '#007791');
    at: #deepGreenishBlue put: (Color fromString: '#2E8495');
    at: #veryLightGreenishBlue  put: (Color fromString: '#9CD1DC');
    at: #lightGreenishBlue put: (Color fromString: '#66AABC');
    at: #moderateGreenishBlue put: (Color fromString: '#367588');
    at: #darkGreenishBlue put: (Color fromString: '#004958');
    at: #veryDarkGreenishBlue put: (Color fromString: '#002E3B');
    at: #vividBlue  put: (Color fromString: '#00A1C2');
    at: #brilliantBlue  put: (Color fromString: '#4997D0');
    at: #strongBlue  put: (Color fromString: '#0067A5');
    at: #deepBlue  put: (Color fromString: '#00416A');
    at: #veryLightBlue  put: (Color fromString: '#a1CAF1');
    at: #lightBlue  put: (Color fromString: '#70A3CC');
    at: #moderateBlue  put: (Color fromString: '#436B95');
    at: #darkBlue  put: (Color fromString: '#00304E');
    at: #veryPaleBlue  put: (Color fromString: '#bCD4E6');
    at: #paleBlue  put: (Color fromString: '#91A3B0');
    at: #grayishBlue  put: (Color fromString: '#536878');
    at: #darkGrayishBlue put: (Color fromString: '#36454F');
    at: #blackishBlue  put: (Color fromString: '#202830');
    at: #bluishWhite  put: (Color fromString: '#e9E9ED');
    at: #lightBluishGray put: (Color fromString: '#b4BCC0');
    at: #bluishGray  put: (Color fromString: '#81878B');
    at: #darkBluishGray put: (Color fromString: '#51585E');
    at: #bluishBlack  put: (Color fromString: '#202428');
    at: #vividPurplishBlue put: (Color fromString: '#30267A');
    at: #brilliantPurplishBlue put: (Color fromString: '#6C79B8');
    at: #strongPurplishBlue put: (Color fromString: '#545AA7');
    at: #deepPurplishBlue put: (Color fromString: '#272458');
    at: #veryLightPurplishBlue  put: (Color fromString: '#b3BCE2');
    at: #lightPurplishBlue put: (Color fromString: '#8791BF');
    at: #moderatePurplishBlue put: (Color fromString: '#4E5180');
    at: #darkPurplishBlue put: (Color fromString: '#252440');
    at: #veryPalePurplishBlue put: (Color fromString: '#c0C8E1');
    at: #palePurplishBlue put: (Color fromString: '#8C92AC');
    at: #grayishPurplishBlue put: (Color fromString: '#4C516D');
    at: #vividViolet  put: (Color fromString: '#9065CA');
    at: #brilliantViolet put: (Color fromString: '#7E73B8');
    at: #strongViolet  put: (Color fromString: '#604E97');
    at: #deepViolet  put: (Color fromString: '#32174D');
    at: #veryLightViolet put: (Color fromString: '#dCD0FF');
    at: #lightViolet  put: (Color fromString: '#8C82B6');
    at: #moderateViolet  put: (Color fromString: '#604E81');
    at: #darkViolet  put: (Color fromString: '#2F2140');
    at: #veryPaleViolet put: (Color fromString: '#c4C3DD');
    at: #paleViolet  put: (Color fromString: '#9690AB');
    at: #grayishViolet  put: (Color fromString: '#554C69');
    at: #vividPurple  put: (Color fromString: '#9A4EAE');
    at: #brilliantPurple put: (Color fromString: '#d399E6');
    at: #strongPurple  put: (Color fromString: '#875692');
 
		yourself! !

!Color class methodsFor: '*nbsiscc-namedcolors' stamp: 'KenD 1/28/2013 19:38'!
nbsisccColors3
	"Answer a dictionary of NBS-ISCC compliant color names to Color objects.
	First half!!"

	^Dictionary new
   at: #deepPurple  put: (Color fromString: '#602F6B');
    at: #veryDeepPurple put: (Color fromString: '#401A4C');
    at: #veryLightPurple put: (Color fromString: '#d5BADB');
    at: #lightPurple  put: (Color fromString: '#b695C0');
    at: #moderatePurple  put: (Color fromString: '#86608E');
    at: #darkPurple  put: (Color fromString: '#563C5C');
    at: #veryDarkPurple put: (Color fromString: '#301934');
    at: #veryPalePurple put: (Color fromString: '#d6CADD');
    at: #palePurple  put: (Color fromString: '#aA98A9');
    at: #grayishPurple  put: (Color fromString: '#796878');
    at: #darkGrayishPurple put: (Color fromString: '#50404D');
    at: #blackishPurple  put: (Color fromString: '#291E29');
    at: #purplishWhite  put: (Color fromString: '#e8E3E5');
    at: #lightPurplishGray put: (Color fromString: '#bFB9BD');
    at: #purplishGray  put: (Color fromString: '#8B8589');
    at: #darkPurplishGray put: (Color fromString: '#5D555B');
    at: #purplishBlack  put: (Color fromString: '#242124');
    at: #vividReddishPurple put: (Color fromString: '#870074');
    at: #strongReddishPurple put: (Color fromString: '#9E4F88');
    at: #deepReddishPurple put: (Color fromString: '#702963');
    at: #veryDeepReddishPurple   put: (Color fromString: '#54194E');
    at: #lightReddishPurple put: (Color fromString: '#b784A7');
    at: #moderateReddishPurple  put: (Color fromString: '#915C83');
    at: #darkReddishPurple put: (Color fromString: '#5D3954');
    at: #veryDarkReddishPurple  put: (Color fromString: '#341731');
    at: #paleReddishPurple put: (Color fromString: '#aA8A9E');
    at: #grayishReddishPurple put: (Color fromString: '#836479');
    at: #brilliantPurplishPink put: (Color fromString: '#fFC8D6');
    at: #strongPurplishPink put: (Color fromString: '#e68FAC');
    at: #deepPurplishPink put: (Color fromString: '#dE6FA1');
    at: #lightPurplishPink put: (Color fromString: '#eFBBCC');
    at: #moderatePurplishPink put: (Color fromString: '#d597AE');
    at: #darkPurplishPink put: (Color fromString: '#c17E91');
    at: #palePurplishPink put: (Color fromString: '#e8CCD7');
    at: #grayishPurplishPink put: (Color fromString: '#c3A6B1');
    at: #vividPurplishRed put: (Color fromString: '#cE4676');
    at: #strongPurplishRed put: (Color fromString: '#b3446C');
    at: #deepPurplishRed put: (Color fromString: '#78184A');
    at: #veryDeepPurplishRed put: (Color fromString: '#54133B');
    at: #moderatePurplishRed put: (Color fromString: '#a8516E');
    at: #darkPurplishRed put: (Color fromString: '#673147');
    at: #veryDarkPurplishRed put: (Color fromString: '#38152C');
    at: #lightGrayishPurplishRed  put: (Color fromString: '#aF868E');
    at: #grayishPurplishRed put: (Color fromString: '#915F6D');
    at: #white   put: (Color fromString: '#f2F3F4');
    at: #lightGray  put: (Color fromString: '#b9B8B5');
    at: #mediumGray  put: (Color fromString: '#848482');
    at: #darkGray  put: (Color fromString: '#555555');
    at: #black  put: (Color fromString: '#222222');
		yourself! !
