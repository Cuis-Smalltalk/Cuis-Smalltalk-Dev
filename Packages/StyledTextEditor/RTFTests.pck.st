'From Cuis 4.1 of 12 December 2012 [latest update: #1759] on 17 July 2013 at 12:57:05.099612 am'!
'Description Please enter a description for this package.'!
!provides: 'RTFTests' 1 0!
!requires: 'RTFExporting' 1 nil!
!requires: 'RTFImporting' 1 nil!
!classDefinition: #RTFConversionTest category: #RTFtests!
TestCase subclass: #RTFConversionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFtests'!
!classDefinition: 'RTFConversionTest class' category: #RTFtests!
RTFConversionTest class
	instanceVariableNames: ''!

!classDefinition: #RTFTokenTest category: #RTFtests!
TestCase subclass: #RTFTokenTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFtests'!
!classDefinition: 'RTFTokenTest class' category: #RTFtests!
RTFTokenTest class
	instanceVariableNames: ''!

!classDefinition: #RTFTokenizerTest category: #RTFtests!
TestCase subclass: #RTFTokenizerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RTFtests'!
!classDefinition: 'RTFTokenizerTest class' category: #RTFtests!
RTFTokenizerTest class
	instanceVariableNames: ''!


!RTFConversionTest methodsFor: 'tests' stamp: 'jmv 4/11/2011 19:45'!
testSample1ToAndFromRTF
	"
	RTFConversionTest new testSample1ToAndFromRTF
	"
	| builder parser rtf text result |
	text _ RTFConversionTest textSample1.
	rtf _ text rtfString.

	builder _ RTFTextBuilder new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	self assert: builder text = text.
	self assert: builder text runs = text runs.

	builder _ RTFTextBuilder currentClass new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	result _ builder text asNonStyledText.
	self assert: result = text.
	self assert: result runs = text runs.! !

!RTFConversionTest methodsFor: 'tests' stamp: 'jmv 4/11/2011 19:45'!
testSample2ToAndFromRTF
	"
	RTFConversionTest new testSample2ToAndFromRTF
	"
	| builder parser rtf text result |
	text _ RTFConversionTest textSample2.
	rtf _ text rtfString.

	builder _ RTFTextBuilder new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.
	self assert: builder text = text.
	self assert: builder text runs = text runs.

	builder _ RTFTextBuilder currentClass new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.
	result _ builder text asNonStyledText.
	self assert: result = text.
	self assert:result runs = text runs.! !

!RTFConversionTest methodsFor: 'tests' stamp: 'jmv 4/11/2011 20:03'!
testSample3ToAndFromRTF
	"
	RTFConversionTest new testSample3ToAndFromRTF
	"
	| builder parser rtf text result |
	text _ RTFConversionTest textSample3.
	rtf _ text rtfString.

	builder _ RTFTextBuilder new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.
	self assert: builder text = text.
	self assert: builder text runs = text runs.

	builder _ RTFTextBuilder currentClass new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.
	result _ builder text asNonStyledText.
	self assert: result = text.
	self assert: result runs = text runs.! !

!RTFConversionTest methodsFor: 'tests' stamp: 'jmv 4/12/2011 10:09'!
testSample4ToAndFromRTF
	"
	RTFConversionTest new testSample4ToAndFromRTF
	"
	| builder parser rtf text result |
	text _ RTFConversionTest textSample4.
	rtf _ text rtfString.

	builder _ RTFTextBuilder new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	self assert: builder text = text.
	"kern and struckThrough not supported in rtf yet..."
"	self assert: builder text runs = text runs."

	builder _ RTFTextBuilder currentClass new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	result _ builder text asNonStyledText.
	self assert: result = text.
	"kern and struckThrough not supported in rtf yet..."
"	self assert: result runs = text runs."! !

!RTFConversionTest methodsFor: 'tests' stamp: 'jmv 4/11/2011 19:45'!
testSample5ToAndFromRTF
	"
	RTFConversionTest new testSample5ToAndFromRTF
	"
	| builder parser rtf text result |
	text _ RTFConversionTest textSample5.
	rtf _ text rtfString.

	builder _ RTFTextBuilder new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	self assert: builder text = text.
	self assert: builder text runs = text runs.

	builder _ RTFTextBuilder currentClass new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	result _ builder text asNonStyledText.
	self assert: result = text.
	self assert: result runs = text runs.! !

!RTFConversionTest methodsFor: 'tests' stamp: 'jmv 11/22/2011 15:27'!
testSample6ToAndFromRTF
	"
	RTFConversionTest new testSample6ToAndFromRTF
	"
	| builder parser rtf text result |
	text _ RTFConversionTest textSample6.
	rtf _ text rtfString.

	builder _ RTFTextBuilder new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	self assert: builder text = text.
	self assert: builder text runs = text runs.

	builder _ RTFTextBuilder currentClass new.
	parser _ RTFParser new.
	parser parseWithTokenizer: (RTFTokenizer newFromString: rtf) buildWith: builder.	
	result _ builder text asNonStyledText.
	self assert: result = text.
	self assert: result runs = text runs.! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfABCExample
	"
	| rtf |
	rtf _ RTFConversionTest rtfABCExample.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1251\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1026\deflangfe1026{\fonttbl{\f0\froman\fcharset204\fprq2{\*\panose 02020603050405020304}Times New Roman;}{\f38\froman\fcharset0\fprq2 Times New Roman;}
{\f36\froman\fcharset238\fprq2 Times New Roman CE;}{\f39\froman\fcharset161\fprq2 Times New Roman Greek;}{\f40\froman\fcharset162\fprq2 Times New Roman Tur;}{\f41\froman\fcharset177\fprq2 Times New Roman (Hebrew);}
{\f42\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f43\froman\fcharset186\fprq2 Times New Roman Baltic;}{\f44\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;
\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;
\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1026\langfe1026\cgrid\langnp1026\langfenp1026 \snext0 Normal;}{\*\cs10 \additive \ssemihidden 
Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\trcbpat1\trcfpat1\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\rsidtbl \rsid1073017\rsid6048286\rsid13708270}
{\*\generator Microsoft Word 10.0.4219;}{\info{\title abc}{\author trifon}{\operator trifon}{\creatim\yr2006\mo5\dy16\hr10\min38}{\revtim\yr2006\mo5\dy16\hr10\min38}{\version1}{\edmins0}{\nofpages1}{\nofwords0}{\nofchars3}{\*\company CLBME}{\nofcharsws3}
{\vern16469}}\paperw11906\paperh16838\margl1417\margr1417\margt1417\margb1417 \deftab708\widowctrl\ftnbj\aenddoc\hyphhotz425\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1417
\dgvorigin1417\dghshow1\dgvshow1\jexpand\viewkind1\viewscale110\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel\wrppunct
\asianbrkrule\rsidroot1073017 \fet0\sectd \linex0\headery708\footery708\colsx708\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang {\pntxta .}}
{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}
{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1026\langfe1026\cgrid\langnp1026\langfenp1026 {\lang1033\langfe1026\langnp1033\insrsid13708270 a}{\b\lang1033\langfe1026\langnp1033\insrsid13708270\charrsid13708270 bc}{
\lang1033\langfe1026\langnp1033\insrsid6048286\charrsid13708270 
\par }}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfColoredSample1
	"
	| rtf |
	rtf _ RTFConversionTest rtfColoredSample1.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1251\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1026\deflangfe1026{\fonttbl{\f0\froman\fcharset204\fprq2{\*\panose 02020603050405020304}Times New Roman{\*\falt Times New Roman};}
{\f211\froman\fcharset0\fprq2 Times New Roman{\*\falt Times New Roman};}{\f209\froman\fcharset238\fprq2 Times New Roman CE{\*\falt Times New Roman};}{\f212\froman\fcharset161\fprq2 Times New Roman Greek{\*\falt Times New Roman};}
{\f213\froman\fcharset162\fprq2 Times New Roman Tur{\*\falt Times New Roman};}{\f214\froman\fcharset177\fprq2 Times New Roman (Hebrew){\*\falt Times New Roman};}{\f215\froman\fcharset178\fprq2 Times New Roman (Arabic){\*\falt Times New Roman};}
{\f216\froman\fcharset186\fprq2 Times New Roman Baltic{\*\falt Times New Roman};}{\f217\froman\fcharset163\fprq2 Times New Roman (Vietnamese){\*\falt Times New Roman};}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;
\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;
\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1026\langfe1026\cgrid\langnp1026\langfenp1026 \snext0 Normal;}{\*\cs10 \additive \ssemihidden 
Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\latentstyles\lsdstimax156\lsdlockeddef0}{\*\rsidtbl \rsid6056135}{\*\generator Micro
soft Word 11.0.6568;}{\info{\title Red green blue}{\author triffon}{\operator triffon}{\creatim\yr2006\mo5\dy5\hr17\min22}{\revtim\yr2006\mo5\dy5\hr17\min29}{\version1}{\edmins7}{\nofpages1}{\nofwords2}{\nofchars13}{\*\company SU}{\nofcharsws14}
{\vern24579}}\paperw11906\paperh16838\margl1417\margr1417\margt1417\margb1417 \deftab708\widowctrl\ftnbj\aenddoc\hyphhotz425\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1417
\dgvorigin1417\dghshow1\dgvshow1\jexpand\viewkind1\viewscale130\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel\wrppunct
\asianbrkrule\rsidroot6056135\newtblstyruls\nogrowautofit \fet0\sectd \linex0\headery708\footery708\colsx708\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2
\pnucltr\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6
\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang 
{\pntxtb (}{\pntxta )}}\pard\plain \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1026\langfe1026\cgrid\langnp1026\langfenp1026 {\cf6\lang1033\langfe1026\langnp1033\insrsid6056135\charrsid6056135 Red}{
\lang1033\langfe1026\langnp1033\insrsid6056135  }{\cf4\lang1033\langfe1026\langnp1033\insrsid6056135\charrsid6056135 green}{\lang1033\langfe1026\langnp1033\insrsid6056135  }{\cf2\lang1033\langfe1026\langnp1033\insrsid6056135\charrsid6056135 blue}{
\lang1033\langfe1026\langnp1033\insrsid6056135\charrsid6056135 
\par }}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfColoredSample2
	"
	| rtf |
	rtf _ RTFConversionTest rtfColoredSample2.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String cStringr)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1252\cocoartf949\cocoasubrtf540
{\fonttbl\f0\froman\fcharset0 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;\red255\green0\blue0;\red0\green255\blue0;\red0\green0\blue255;
}
\deftab708
\pard\pardeftab708\ri-432

\f0\fs24 \cf2 Red\cf0  \cf3 green\cf0  \cf4 blue}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfSample1
	"
	| rtf |
	rtf _ RTFConversionTest rtfSample1.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1251\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1026\deflangfe1026{\fonttbl{\f0\froman\fcharset204\fprq2{\*\panose 02020603050405020304}Times New Roman;}
{\f1\fswiss\fcharset204\fprq2{\*\panose 020b0604020202020204}Arial;}{\f92\froman\fcharset0\fprq2 Times New Roman;}{\f90\froman\fcharset238\fprq2 Times New Roman CE;}{\f93\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f94\froman\fcharset162\fprq2 Times New Roman Tur;}{\f95\froman\fcharset177\fprq2 Times New Roman (Hebrew);}{\f96\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f97\froman\fcharset186\fprq2 Times New Roman Baltic;}
{\f98\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}{\f102\fswiss\fcharset0\fprq2 Arial;}{\f100\fswiss\fcharset238\fprq2 Arial CE;}{\f103\fswiss\fcharset161\fprq2 Arial Greek;}{\f104\fswiss\fcharset162\fprq2 Arial Tur;}
{\f105\fswiss\fcharset177\fprq2 Arial (Hebrew);}{\f106\fswiss\fcharset178\fprq2 Arial (Arabic);}{\f107\fswiss\fcharset186\fprq2 Arial Baltic;}{\f108\fswiss\fcharset163\fprq2 Arial (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;
\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;
\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1026\langfe1026\cgrid\langnp1026\langfenp1026 \snext0 Normal;}{\*\cs10 \additive 
\ssemihidden Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\rsidtbl \rsid670965\rsid4534232\rsid4597070\rsid8857962\rsid13514577\rsid13519090}
{\*\generator Microsoft Word 10.0.4219;}{\info{\title aaabbbccc}{\author kalin}{\operator kalin}{\creatim\yr2006\mo4\dy29\hr15\min39}{\revtim\yr2006\mo4\dy30\hr18\min47}{\version8}{\edmins1}{\nofpages1}{\nofwords4}{\nofchars29}{\*\company HCData}
{\nofcharsws32}{\vern16469}}\paperw11906\paperh16838\margl1417\margr1417\margt1417\margb1417 \deftab708\widowctrl\ftnbj\aenddoc\hyphhotz425\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\hyphcaps0\formshade\horzdoc\dgmargin\dghspace180
\dgvspace180\dghorigin1417\dgvorigin1417\dghshow1\dgvshow1
\jexpand\viewkind1\viewscale100\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel\wrppunct\asianbrkrule\rsidroot4534232 \fet0\sectd 
\linex0\headery708\footery708\colsx708\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang
{\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl7
\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1026\langfe1026\cgrid\langnp1026\langfenp1026 {\b\i\lang1033\langfe1026\langnp1033\insrsid670965\charrsid670965 aaa}{\lang1033\langfe1026\langnp1033\insrsid670965 bbb}{
\b\lang1033\langfe1026\langnp1033\insrsid670965\charrsid670965 ccc}{\lang1033\langfe1026\langnp1033\insrsid8857962 
\par }{\lang1033\langfe1026\langnp1033\insrsid670965 
\par ddd}{\f1\lang1033\langfe1026\langnp1033\insrsid670965\charrsid670965 eee}{\lang1033\langfe1026\langnp1033\insrsid670965 fff 
\par ggg}{\b\i\f1\lang1033\langfe1026\langnp1033\insrsid670965\charrsid670965 hhh}{\lang1033\langfe1026\langnp1033\insrsid670965 iii 
\par }} 
'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfSrcCharFormat
	"
	| rtf |
	rtf _ RTFConversionTest rtfSrcCharFormat.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\mac\ansicpg10000\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1033\deflangfe1033{\upr{\fonttbl{\f0\fnil\fcharset256\fprq2{\*\panose 00020206030504050203}Times New Roman;}
{\f1\fnil\fcharset256\fprq2{\*\panose 00020b06040202020202}Arial;}}{\*\ud{\fonttbl{\f0\fnil\fcharset256\fprq2{\*\panose 00020206030504050203}Times New Roman;}{\f1\fnil\fcharset256\fprq2{\*\panose 00020b06040202020202}Arial;}}}}
{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;
\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 
\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}{\*\cs10 \additive Default Paragraph Font;}{\*
\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\trcbpat1\trcfpat1\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 Normal Table;}{\s15\ql \li0\ri0\widctlpar\tqc\tx4320\tqr\tx8640\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 
\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \sbasedon0 \snext15 header;}}{\*\rsidtbl \rsid12155552}{\info{\title This test is written using \''d2Microsoft Word Test Drive\''d3 for Mac}{\author Office 2004 Test Drive User}
{\operator Office 2004 Test Drive User}{\creatim\yr2006\mo5\dy6\min58}{\revtim\yr2006\mo5\dy6\min58}{\version2}{\edmins0}{\nofpages1}{\nofwords0}{\nofchars0}{\nofcharsws0}{\vern24835}}\paperw11900\paperh16840 
\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dghspace180\dgvspace180\dghorigin1701\dgvorigin1984\dghshow0\dgvshow0
\jexpand\viewkind1\viewscale130\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\rsidroot16320125 \fet0\sectd \linex0\endnhere\sectdefaultcl\sectrsid12517500\sftnbj 
{\header \pard\plain \s15\ql \li0\ri0\widctlpar\tqc\tx4320\tqr\tx8640\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid12517500 
\par }}{\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang{\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang{\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang{\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang{\pntxta )}}
{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl8
\pnlcltr\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}\pard\plain \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 
\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid12517500 This test is written using \ldblquote Microsoft Word Test Drive\rdblquote  for Mac. It is shows some aspects of the character formattings, like: }{
\b\cf6\insrsid12517500\charrsid12517500 Color}{\insrsid12517500 , }{\b\insrsid12517500\charrsid12517500 Bold}{\insrsid12517500 , }{\b\i\insrsid12517500\charrsid12517500 Bold Italic}{\insrsid12517500 , }{\i\insrsid12517500\charrsid12517500 Italic}{
\insrsid12517500 , and }{\ul\insrsid12517500\charrsid12517500 Underlining}{\insrsid12517500\charrsid12517500 .}{\ul\insrsid12517500\charrsid12517500  }{\insrsid12517500\charrsid12517500 
\par }{\f1\fs28\insrsid12517500\charrsid12517500 This is the Arial font, sized 14}{\insrsid12517500\charrsid12517500 . 
\par }}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfSrcParagraphFormatting
	"
	| rtf |
	rtf _ RTFConversionTest rtfSrcParagraphFormatting.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\mac\ansicpg10000\cocoartf824\cocoasubrtf350
{\fonttbl\f0\froman\fcharset77 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;}
{\info
{\title This is a test of all formatting needed}
{\author Compay Sedungo}
{\*\company WAY AHEAD}}\vieww9200\viewh8100\viewkind1\viewscale100
\deftab720
\pard\pardeftab720\ri0

\f0\fs24 \cf0 This is a test of almost all paragraph formatting needed.\
\pard\pardeftab720\fi7200\ri0\sb400
\cf0 This is left aligned and with 20pt before. And has indentFirst\
\pard\pardeftab720\ri0\sa300\qc
\cf0 This is centered and with 15pt after\
\pard\pardeftab720\ri0\qr
\cf0 This is right aligned\
\pard\pardeftab720\li1440\ri2880\qj
\cf0 This is justified and with left and right indents\
\pard\pardeftab720\ri0
\cf0 \
}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfSrcPrefaceHalsOriginal
	"
	| rtf |
	rtf _ RTFConversionTest rtfSrcPrefaceHalsOriginal.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\mac\ansicpg10000\uc1 \deff4\deflang1033\deflangfe1033{\upr{\fonttbl{\f0\fnil\fcharset256\fprq2{\*\panose 00020206030504050203}Times New Roman;}{\f4\fnil\fcharset256\fprq2{\*\panose 00020005000000000000}Times;}
{\f5\fnil\fcharset256\fprq2{\*\panose 00020005000000000000}Helvetica;}{\f216\fnil\fcharset256\fprq2{\*\panose 0204002c795c00000002}Palex{\*\falt Times New Roman};}}{\*\ud{\fonttbl{\f0\fnil\fcharset256\fprq2{\*\panose 00020206030504050203}Times New Roman;}
{\f4\fnil\fcharset256\fprq2{\*\panose 00020005000000000000}Times;}{\f5\fnil\fcharset256\fprq2{\*\panose 00020005000000000000}Helvetica;}{\f216\fnil\fcharset256\fprq2{\*\panose 0204002c795c00000002}Palex{\*\falt Times New Roman};}}}}
{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;
\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\widctlpar\faauto\adjustright\rin0\lin0\itap0 \f4\lang1033\cgrid \snext0 Normal;}{\*\cs10 \additive Default Paragraph Font;}
{\*\cs15 \additive \ul\cf2 \sbasedon10 Hyperlink;}{\*\cs16 \additive \ul\cf12 \sbasedon10 FollowedHyperlink;}}{\info{\title PREFACE}{\author Bob Stein}{\operator dan visel}{\creatim\yr1999\mo6\dy20\hr10\min39}{\revtim\yr2006\mo4\dy12\hr8\min2}{\version6}
{\edmins22}{\nofpages4}{\nofwords1299}{\nofchars7408}{\nofcharsws9097}{\vern16525}}\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\hyphcaps0\formshade\horzdoc\dgmargin\dghspace120\dgvspace120\dghorigin1701\dgvorigin1984\dghshow0
\dgvshow3\jcompress\viewkind4\viewscale150\pgbrdrhead\pgbrdrfoot\nolnhtadjtbl \fet0\sectd \linex0\endnhere\sectdefaultcl {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang{\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang{\pntxta .}}{\*\pnseclvl3
\pndec\pnstart1\pnindent720\pnhang{\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang{\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}
{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}\pard\plain \sl-340\slmult0
\widctlpar\faauto\adjustright\rin0\lin0\itap0 \f4\lang1033\cgrid {\i\f216\fs28\cf1 I am a HAL Nine Thousand computer, Production Number 3. I became operational at the HAL Plant in Urbana, Illinois, on January 12, 1997. }{\f216\fs28\cf1  }{\fs28\cf1 
\par }\pard \qr\sl-340\slmult0\widctlpar\faauto\adjustright\rin0\lin0\itap0 {\f216\cf1 - }{\i\f216\cf1 HAL, 2001: A Space Odyssey }{\f216\cf1 (the novel)  
\par }\pard \sl-340\slmult0\widctlpar\faauto\adjustright\rin0\lin0\itap0 {\cf1 
\par }{\f216\cf1 At a dinner party some time ago, an acquaintance, a nonscientist, asked me
 in a casual way about my duties as chief scientist at a research lab. I said that one of my great joys was overseeing a wide range of projects, to varying extents, and I mentioned a few of them: pattern recognition, machine learning, neural networks, com
p
uter-chip design, supercomputer design, image compression, expert systems, handwriting recognition, document analysis, uses of global networks such as the World Wide Web, novel human-machine interfaces, and so on. Then I turned to one of the areas of my p
articular expertise: lipreading by computer. 
\par }{\cf1 
\par }{\f216\cf1 "Oh," she said, "Like HAL." Ah, a kindred soul, I thought. We spent quite some time discussing the state of the art and the challenges of computer lipreading, its possible applications, and so on. Later our dis
cussion turned to other topics suggested by the movie - language understanding, chess, computer vision, artificial intelligence. It was clear that she was interested in the current state of the art and that many years before the film had both caught her i
magination and helped her identify crucial issues in today''s computer science. One of the questions she asked was, "How realistic was HAL?" 
\par }{\cf1 
\par }{\f216\cf1 This book is for people like her. And because no one is an expert in all the topics covered in the film, even scien
tists are sure to learn from the accounts of other areas. The book is much more than an answer to her question, though. It has four major goals, which it addresses in varying proportions in the sixteen chapters. 
\par 
\par }{\b\f5\cf1 Analysis 
\par }{\f216\cf1 It is a testament to Clarke and 
Kubrick''s achievement that 2001 still holds up to close scrutiny in the late 1990s. Under the expert eyes of the contributors, the most innocuous aspects of scenes - a line of computer code on a screen, a chess move, the use of a word, the form of a butto
n - reveal a great deal. Even though I''ve seen the film several dozen times, I have learned an immense amount from the contributors. }{\i\f216\cf1 HAL''s Legacy}{\f216\cf1  seeks to do for }{\i\f216\cf1 2001}{\f216\cf1 
 what good art history does for a major painting; namely, make the viewer see it in a new light - a tall order, to be sure!! 
\par 
\par }{\b\f5\cf1 Teaching}{\f5\cf1  
\par }{\f216\cf1 The film illustrates key ideas in several disciplines of computer science, and thus provides a springboard for discussions of the field in greater depth, including our own research. Descriptions of the world com
puter chess champion Deep Blue system, the commercially successful VOICE recognition system, the massive CYC artificial-intelligence project, the award-winning Mathematica software system, and much more are here discussed by their creators at a level acce
ssible to the general reader. 
\par 
\par }{\b\f5\cf1 Prognostication }{\f5\cf1  
\par }{\f216\cf1 It is natural, too, to look to the future. Several contributors make informed and fascinating predictions based on developments in the field. What are the most promising approaches toward artificial intellig
ence? Will we ever be able to "reverse engineer" a human brain and represent it in a computer? 
\par 
\par }{\b\f5\cf1 Reflection }{\f5\cf1  
\par }{\i\f216\cf1 2001}{\f216\cf1  transcends the label of "science fiction movie" and captures many of the central metaphors of our time, telling us much about society and its
 aspirations. The film has even been praised by the pope!! Many people have been deeply affected by the film, among them several contributors who reflect here about its influence on their own careers and on computer science in general. 
\par }{\cf1 
\par }{\f216\cf1 Clearly, }{\i\f216\cf1 HAL''s Legacy}{\f216\cf1 
 differs from books on the making of the film or its cinematography. It differs, too, from books that analyze the science shown in movies or on television - science that is incidental and just "goes along for the ride." To an extent unprecedented and nev
er duplicated in a feature film, the makers of 2001 were as careful as possible to get things right; when they did make errors, they often did so in illuminating ways. 
\par 
\par Now seems like the perfect time for }{\i\f216\cf1 HAL''s Legacy.}{\f216\cf1 
 Birthdays are an important theme in the film (there are at least five of them), and in the novel, HAL "becomes operational ... on January 12, 1997." Kubrick changed the year to 1992 for the film version - perhaps to give HAL a longer lifetime and so make
 
his death more poignant. On the 1992 date, I - along with colleagues, faculty, and assorted Silicon Valley friends - held a birthday party for HAL. I was interviewed by several papers, and an Associated Press photo of me cutting the HAL cake (shaped like 
his console, complete with red LED under a clear plastic hemisphere) appeared worldwide. I was pleasantly surprised to learn that much of the general public was interested in HAL too. 
\par 
\par It has been particularly rewarding for me to work with this group of co
ntributors - all of whom were chosen because of their preeminence in their respective subfields. I have known a few of them personally for many years; Azriel Rosenfeld was on my dissertation committee. Others I met serving on panel discussions. I''ll never
 
forget the time I came dressed in a suit while fellow panelist Marvin Minsky showed up in a Pac Man T-shirt. Yet others I knew primarily through their books - Dan Dennett and Don Norman, for example - and still others are inventors of products I use regul
a
rly (e.g., Steve Wolfram''s Mathematica). At our meetings and dinners in Stanford, Urbana, and Cambridge, and through frequent written messages, we passed many ideas back and forth. Although I had strong ideas about what I wanted them to write, they all ha
d the good sense to ignore me when appropriate. At times I felt like someone trying to herd cats. 
\par }{\cf1 
\par }{\f216\cf1 Even at a distance, there was a great sense of camaraderie. As we approached one of the important publishing deadlines, one contributor, who was still late w
ith a chapter, replied to my frantic entreaties thus: "Dave, I honestly think you ought to sit down calmly, take a stress pill and think things over." A later message read, "I still have every confidence in the success of my chapter," which at first broug
ht bemusement but then a diffuse sense of dread. 
\par 
\par It has been a privilege to correspond with Arthur C. Clarke, whose work inspired us all. Throughout the preparation of this book he has been gracious, enthusiastic, and helpful. 
\par }{\cf1 
\par }{\f216\cf1 Although I did my writing 
and editing at home, often late into the night and on weekends, I would like to thank my colleagues at the Ricoh California Research Center for their support of our ongoing research, which influenced this book in numerous ways: Greg Wolff, K.V. Prasad, Mi
c
hael Angelo (yes, that''s his real name), Morten Pedersen (visiting from the Technical University of Denmark), Stanford graduate students Vicky Lu, Chuck Lam, and (especially) Marcus Hennecke (by the time this book is released, Dr. Hennecke!!). Thanks also 
go to Director Peter Hart for making CRC such a great place to work. 
\par }{\cf1 
\par }{\f216\cf1 This book was improved indirectly by a large number of people. One colleague pointed out a used bookstore selling an out-of-print book about the filming of 2001; an acquaintance asked a 
"naive" question that ultimately led to a new section in a chapter; a student told me about a 2001 World Wide Web site; an intrepid cab driver took me through the blizzard of ''96 to interview Marvin Minsky. Piers Bizony, whose book on the filming of 2001 
b
oth inspired and informed me, made several transatlantic phone calls and helped me track down photographs. I also had a somewhat eerie telephone conversation with Douglas Rain, the Canadian actor who played the voice of HAL. Thanks go also to the efficien
t staff at Turner Broadcasting for their assistance providing stills from the film. 
\par }{\cf1 
\par }{\f216\cf1 An extra-special thanks goes to my editor at the MIT Press, Bob Prior. He was the only person in the publishing industry who "got" the idea of HAL''s Legacy instantly, as p
roven by his enthusiastic response to my proposal. Michael Rutter, also at the Press, helped obtain illustrations and kept track of numerous production details. Sandra Minkkinen helped to orchestrate the editing and production process for the entire proje
ct, and copy editor Roberta Clark improved the text immeasurably. 
\par }{\cf1 
\par }{\f216\cf1 Deep appreciation goes to my immediate family - Nancy, Alex, and Olivia - for putting up with my many late nights and weekend hours working on the book. I am happy to say that groggy Saturd
ay mornings after marathon editing sessions are now a thing of the past, and we can spend more time doing what we all love so much: hiking Mount Tamalpais and the Marin headlands and kayaking on Squibnocket Pond. 
\par }{\cf1 
\par }{\f216\cf1 David G. Stork
\par Stanford, California
\par January 12, 1996 }{\cf1 
\par 
\par 
\par }{\f216\cf1 UCSC Perpetual Science Laboratory  }{\field{\*\fldinst {\f216\cf1  HYPERLINK "http://mambo.ucsc.edu/" }{\f216\cf1 {\*\datafield 
00d0c9ea79f9bace118c8200aa004ba90b02000000170000001700000068007400740070003a002f002f006d0061006d0062006f002e0075006300730063002e006500640075002f000000e0c9ea79f9bace118c8200aa004ba90b2e00000068007400740070003a002f002f006d0061006d0062006f002e00750063007300
63002e006500640075002f000000}}}{\fldrslt {\cs15\f216\ul\cf2 http://mambo.ucsc.edu/}}}{\f216\cf1  
\par Ricoh''s California Research Center }{\field{\*\fldinst {\f216\cf1  HYPERLINK "http://www.crc.ricoh.com/" }{\f216\cf1 {\*\datafield 
00d0c9ea79f9bace118c8200aa004ba90b0200000003000000e0c9ea79f9bace118c8200aa004ba90b3400000068007400740070003a002f002f007700770077002e006300720063002e007200690063006f0068002e0063006f006d002f000000}}}{\fldrslt {\cs15\f216\ul\cf2 http://www.crc.ricoh.com/}}}{
\cf1 
\par 
\par 
\par }{
\par }}'
! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:45'!
rtfSrcSpacing
	"
	| rtf |
	rtf _ RTFConversionTest rtfSrcSpacing.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1252\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1033\deflangfe1033{\fonttbl{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}{\f81\froman\fcharset238\fprq2 Times New Roman CE;}
{\f82\froman\fcharset204\fprq2 Times New Roman Cyr;}{\f84\froman\fcharset161\fprq2 Times New Roman Greek;}{\f85\froman\fcharset162\fprq2 Times New Roman Tur;}{\f86\froman\fcharset177\fprq2 Times New Roman (Hebrew);}
{\f87\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f88\froman\fcharset186\fprq2 Times New Roman Baltic;}{\f89\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;
\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;
\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}{\*\cs10 \additive \ssemihidden 
Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\latentstyles\lsdstimax156\lsdlockeddef0}{\*\rsidtbl \rsid9840235}{\*\generator Micro
soft Word 11.0.6568;}{\info{\title This example is a test of line spacing}{\author Compay Sedungo}{\operator Compay Sedungo}{\creatim\yr2006\mo5\dy4\hr10\min14}{\revtim\yr2006\mo5\dy4\hr10\min18}{\version1}{\edmins4}{\nofpages1}{\nofwords153}
{\nofchars873}{\*\company WAY AHEAD}{\nofcharsws1024}{\vern24579}}\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1800\dgvorigin1440\dghshow1\dgvshow1
\jexpand\viewkind1\viewscale100\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\viewnobound1\snaptogridincell\allowfieldendsel
\wrppunct\asianbrkrule\rsidroot9840235\newtblstyruls\nogrowautofit \fet0\sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang 
{\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang 
{\pntxtb (}{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid9840235 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid9840235 This example is a test of line spacing. 
Line spacing is the space between lines in a paragraph. Sometimes it is also called paragraph leading. This paragraph is single spaced.
\par }\pard \ql \li0\ri0\sl360\slmult1\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid9840235 {\insrsid9840235 This example is a test of line spacing. Line spacing is the space between lines in a paragraph. 
Sometimes it is also called paragraph leading. This is 1.5-spaced
\par }\pard \ql \li0\ri0\sl480\slmult1\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid9840235 {\insrsid9840235 This example is a test of line spacing. Line spacing is the space between lines in a paragraph. 
Sometimes it is also called paragraph leading. This is double-spaced
\par }\pard \ql \li0\ri0\sl240\slmult0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid9840235 {\insrsid9840235 This example is a test of line spacing. Line spacing is the space between lines in a paragraph. 
Sometimes it is also called paragraph leading. This is at lease 12pt
\par }\pard \ql \li0\ri0\sl-240\slmult0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid9840235 {\insrsid9840235 This example is a test of line spacing. Line spacing is the space between lines in a paragraph. 
Sometimes it is also called paragraph leading. This is exactly 12pt.
\par }\pard \ql \li0\ri0\sl720\slmult1\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid9840235 {\insrsid9840235 This example is a test of line spacing. Line spacing is the space between lines in a paragraph. 
Sometimes it is also called paragraph leading. This is spaced by a multiple of a line \endash  3 times.
\par }}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:46'!
rtfSrcSpecialSymbols
	"
	| rtf |
	rtf _ RTFConversionTest rtfSrcSpecialSymbols.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1252\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1033\deflangfe1033{\fonttbl{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}{\f71\froman\fcharset238\fprq2 Times New Roman CE;}

{\f72\froman\fcharset204\fprq2 Times New Roman Cyr;}{\f74\froman\fcharset161\fprq2 Times New Roman Greek;}{\f75\froman\fcharset162\fprq2 Times New Roman Tur;}{\f76\froman\fcharset177\fprq2 Times New Roman (Hebrew);}

{\f77\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f78\froman\fcharset186\fprq2 Times New Roman Baltic;}{\f79\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;

\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;

\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}{\*\cs10 \additive \ssemihidden 

Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 

\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\latentstyles\lsdstimax156\lsdlockeddef0}{\*\rsidtbl \rsid11999034}{\*\generator Micr

osoft Word 11.0.6568;}{\info{\title Tabulation}{\author Compay Sedungo}{\operator Compay Sedungo}{\creatim\yr2006\mo5\dy3\hr10\min7}{\revtim\yr2006\mo5\dy3\hr10\min8}{\version1}{\edmins1}{\nofpages1}{\nofwords8}{\nofchars48}{\*\company WAY AHEAD}

{\nofcharsws55}{\vern24579}}\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1800\dgvorigin1440\dghshow1\dgvshow1

\jexpand\viewkind1\viewscale100\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel\wrppunct

\asianbrkrule\rsidroot11999034\newtblstyruls\nogrowautofit \fet0\sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang {\pntxta .}}

{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}

{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 

\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid11999034 Tabulation\tab tabulation

\par Return

\par Return

\par Softbreak\line softbreak

\par }}'! !

!RTFConversionTest class methodsFor: 'rtf examples' stamp: 'jmv 3/13/2012 11:46'!
rtfSrcWin32ClipboardTest
	"
	| rtf |
	rtf _ RTFConversionTest rtfSrcWin32ClipboardTest.
	'Works on the mac'.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (rtf withLineEndings: String crString)  dataFormat: 'public.rtf'
	"
	^'{\rtf1\ansi\ansicpg1251\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1026\deflangfe1026{\fonttbl{\f0\froman\fcharset204\fprq2{\*\panose 02020603050405020304}Times New Roman;}

{\f38\fswiss\fcharset204\fprq2{\*\panose 020b0604030504040204}Verdana;}{\f174\froman\fcharset0\fprq2 Times New Roman;}{\f172\froman\fcharset238\fprq2 Times New Roman CE;}{\f175\froman\fcharset161\fprq2 Times New Roman Greek;}

{\f176\froman\fcharset162\fprq2 Times New Roman Tur;}{\f177\froman\fcharset177\fprq2 Times New Roman (Hebrew);}{\f178\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f179\froman\fcharset186\fprq2 Times New Roman Baltic;}

{\f180\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}{\f554\fswiss\fcharset0\fprq2 Verdana;}{\f552\fswiss\fcharset238\fprq2 Verdana CE;}{\f555\fswiss\fcharset161\fprq2 Verdana Greek;}{\f556\fswiss\fcharset162\fprq2 Verdana Tur;}

{\f559\fswiss\fcharset186\fprq2 Verdana Baltic;}{\f560\fswiss\fcharset163\fprq2 Verdana (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;

\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{

\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 \styrsid8157421 Normal,DO NOT USE,n;}{

\s4\ql \li0\ri0\sb100\sa60\keepn\widctlpar\aspalpha\aspnum\faauto\outlinelevel3\adjustright\rin0\lin0\itap0 \b\f38\fs18\expnd-2\expndtw-10\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \sbasedon0 \snext0 \styrsid8157421 heading 4,H4;}{\*\cs10 

\additive \ssemihidden Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 

\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}{\s15\ql \li0\ri0\sa120\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 

\f38\fs20\lang1033\langfe1033\langnp1033\langfenp1033 \sbasedon0 \snext15 \slink16 \styrsid8157421 Body Text;}{\*\cs16 \additive \f38\lang1033\langfe1033\cgrid0\langnp1033\langfenp1033 \sbasedon10 \slink15 \slocked \styrsid8157421 Body Text Char;}}

{\*\latentstyles\lsdstimax156\lsdlockeddef0}{\*\rsidtbl \rsid8157421}\margl1417\margr1417\margt1417\margb1417 \deftab708\widowctrl\ftnbj\aenddoc\hyphhotz425\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180

\dgvspace180\dghorigin1701\dgvorigin1984\dghshow1\dgvshow1

\jexpand\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel\wrppunct\asianbrkrule\rsidroot8157421\newtblstyruls\nogrowautofit \fet0\sectd 

\linex0\headery708\footery708\colsx708\endnhere\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}

{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang 

{\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 

\s4\ql \li0\ri0\sb100\sa60\keepn\widctlpar\aspalpha\aspnum\faauto\outlinelevel3\adjustright\rin0\lin0\itap0\pararsid8157421 \b\f38\fs18\expnd-2\expndtw-10\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid8157421 {\*\bkmkstart _Toc335399101}

{\*\bkmkstart _Ref335401498}{\*\bkmkstart _Ref335402606}{\*\bkmkstart _Ref335402655}{\*\bkmkstart _Ref335402668}{\*\bkmkstart _Ref335402836}{\*\bkmkstart _Ref335403019}{\*\bkmkstart _Ref335403299}{\*\bkmkstart _Ref335403350}{\*\bkmkstart _Toc380819805}

{\*\bkmkstart Special_Characters}Special Characters{\*\bkmkend _Toc335399101}{\*\bkmkend _Ref335401498}{\*\bkmkend _Ref335402606}{\*\bkmkend _Ref335402655}{\*\bkmkend _Ref335402668}{\*\bkmkend _Ref335402836}{\*\bkmkend _Ref335403019}

{\*\bkmkend _Ref335403299}{\*\bkmkend _Ref335403350}{\*\bkmkend _Toc380819805}{\*\bkmkend Special_Characters}

\par }\pard\plain \s15\ql \li0\ri0\sa120\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0\pararsid8157421 \f38\fs20\lang1033\langfe1033\langnp1033\langfenp1033 {\insrsid8157421 

The RTF Specification includes control words for special characters (described as <spec> in the character-text syntax description). If 

a special-character control word is not recognized by the RTF reader, it is ignored and the text following it is considered plain text. The RTF Specification is flexible enough to allow new special characters to be added for interchange with other softwar

e.

\par The special RTF characters are listed in the following table.

\par }} '! !

!RTFConversionTest class methodsFor: 'text samples' stamp: 'jmv 4/11/2011 20:06'!
textSample1
	"
	| text |
	text _ RTFConversionTest textSample1.
	text edit.
	Clipboard default storeObject: text
	"
	^
		(Text string: 'normal '),
		(Text string: 'bold ' attribute: (TextEmphasis bold)),
		(Text string: 'boldItalic ' attributes: {TextEmphasis new emphasisCode: AbstractFont boldCode + AbstractFont italicCode}),
		(Text string: 'italic ' attributes: {TextEmphasis italic}),
		(Text string: 'boldGreen ' attributes: {TextEmphasis bold. TextColor green}),
		(Text string: 'red ' attributes: {TextColor red}),
		(Text string: 'underlined ' attribute: (TextEmphasis underlined)),
		(Text string: 'normal ' attributes: #())! !

!RTFConversionTest class methodsFor: 'text samples' stamp: 'jmv 3/13/2012 16:57'!
textSample2
	"
	| text |
	text _ RTFConversionTest textSample2.
	text edit.
	Clipboard default storeObject: text
	"
	^
		(Text string: 'This text has no tyle set', String newLineString),
		(Text string: 'This is centered', String newLineString attribute: TextAlignment centered),
		(Text string: 'This text has no tyle set', String newLineString),
		(Text string: 'This is right', String newLineString attribute: TextAlignment rightFlush),
		(Text string: 'This text has no tyle set', String newLineString),
		(Text string: 'This is justified, and to see the effect, the paragraph needs to span over several lines, ',
			'as, otherwise, you won"t be able to see the effect', String newLineString attribute: TextAlignment justified),
		(Text string: 'This text has no tyle set', String newLineString),
		(Text string: 'This is left', String newLineString),
		(Text string: 'This text has no tyle set', String newLineString),
		(Text string: 'This is right', String newLineString attribute: TextAlignment rightFlush)! !

!RTFConversionTest class methodsFor: 'text samples' stamp: 'jmv 4/11/2011 20:06'!
textSample3
	"
	| text |
	text _ RTFConversionTest textSample3.
	text edit.
	Clipboard default storeObject: text
	"
	^ (
		(Text string: 'normal '),
		(Text string: 'bold ' attributes: {(TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 17). TextEmphasis bold}),
		(Text string: 'italic ' attributes: {(TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 6). TextEmphasis italic}),
		(Text string: 'boldGreen ' attributes: {(TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 12). TextEmphasis bold. TextColor green}),
		(Text string: 'red ' attributes: {TextColor red}),
		(Text string: 'underlined ' attribute: (TextEmphasis underlined)),
		(Text string: 'normal ' attributes: #())
	)! !

!RTFConversionTest class methodsFor: 'text samples' stamp: 'jmv 4/12/2011 10:00'!
textSample4
	"
	| text |
	text _ RTFConversionTest textSample4.
	text edit.
	Clipboard default storeObject: text
	"
	^ (
		(Text string: 'normal '),
		(Text string: 'bold ' attributes: { (TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 17). TextEmphasis bold }),
		(Text string: 'italic ' attributes: { (TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 6). TextEmphasis italic }),
		(Text string: 'boldGreen ' attributes: { (TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 12). TextEmphasis bold. TextColor green }),
		(Text string: 'boldGreen ' attributes: { (TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 12). TextEmphasis bold. TextColor green. TextKern kern: 2}),
		(Text string: 'boldGreen ' attributes: { (TextFontFamilyAndSize familyName: 'DejaVu' pointSize: 12). TextEmphasis bold. TextColor green. TextKern kern: -2}),
		(Text string: 'red ' attributes: {TextColor red}),
		(Text string: 'underlined ' attribute: (TextEmphasis underlined)),
		(Text string: 'struckThrough ' attribute: (TextEmphasis struckThrough)),
		(Text string: 'normal ' attributes: #())
	)! !

!RTFConversionTest class methodsFor: 'text samples' stamp: 'jmv 12/7/2012 22:38'!
textSample5
	"
	| text |
	text _ RTFConversionTest textSample5.
	text edit.
	Clipboard default storeObject: text
	"
	^'Hello', (Text withForm: (EllipseMorph new imageForm: 32)), 'world'! !

!RTFConversionTest class methodsFor: 'text samples' stamp: 'jmv 11/22/2011 15:00'!
textSample6
	"
	| text |
	text _ RTFConversionTest textSample6.
	text edit.
	Clipboard default storeObject: text
	"
	^'Tomá agüita, ñandú. Ñandú. 
½´®¥¨øåßƒ©Ý²Þ¬­ç¦µ.
¼´®Á¨ØÅÍÎÏ©ÓÔÞÒ­Ç¦ÐÂ' asText! !

!RTFTokenTest methodsFor: 'as yet unclassified' stamp: 'MR 5/24/2006 12:10'!
testCreateBlockClose
	| token |	

	token _ RTFToken newBlockClose.
	self
		assert: (token type = #blockClose);
		assert: (token string = nil);
		assert: (token word = nil);
		assert: (token arg = nil).
! !

!RTFTokenTest methodsFor: 'as yet unclassified' stamp: 'MR 5/24/2006 12:10'!
testCreateBlockOpen
	| token |	

	token _ RTFToken newBlockOpen.
	self
		assert: (token type = #blockOpen);
		assert: (token string = nil);
		assert: (token word = nil);
		assert: (token arg = nil).
! !

!RTFTokenTest methodsFor: 'as yet unclassified' stamp: 'MR 5/24/2006 12:11'!
testCreateKeyword
	| token |	

	token _ RTFToken newKeyword: 'abcd'.
	self
		assert: (token type = #keyword);
		assert: (token string = nil);
		assert: (token word = 'abcd');
		assert: (token arg = 0).

	token _ RTFToken newKeyword: 'abcd' withArg: 10.
	self
		assert: (token type = #keyword);
		assert: (token string = nil);
		assert: (token word = 'abcd');
		assert: (token arg = 10).
! !

!RTFTokenTest methodsFor: 'as yet unclassified' stamp: 'MR 5/24/2006 12:11'!
testCreateString
	| token |	

	token _ RTFToken newString: 'abcd'.
	self
		assert: (token type = #string);
		assert: (token string = 'abcd');
		assert: (token word = nil);
		assert: (token arg = nil).

! !

!RTFTokenizerTest methodsFor: 'as yet unclassified' stamp: 'MR 5/25/2006 18:44'!
testMacGetToken
	| string tokenizer token |

	"string is an empty rtf generated by word 2003"	
	string _ '{\rtf1\mac\ansicpg10000\cocoartf824\cocoasubrtf350
{\fonttbl}
{\colortbl;\red255\green255\blue255;}
\paperw11900\paperh16840\margl1440\margr1440\vieww9000\viewh8400\viewkind0
}'.

	tokenizer _ RTFTokenizer newFromString: string.

	"start parsing check"
	self assert: tokenizer getToken isBlockOpen.
	token _ tokenizer getToken.
	self assert: (token word = 'rtf');
		assert: (token arg = 1).
	token _ tokenizer getToken.
	self assert: (token word = 'mac').
	token _ tokenizer getToken.
	self assert: (token word = 'ansicpg');
		assert: (token arg = 10000).
	token _ tokenizer getToken.
	self assert: (token word = 'cocoartf');
		assert: (token arg = 824).
	token _ tokenizer getToken.
	self assert: (token word = 'cocoasubrtf');
		assert: (token arg = 350).
	self assert: tokenizer getToken isBlockOpen.
	token _ tokenizer getToken.
	self assert: (token word = 'fonttbl').
	self assert: tokenizer getToken isBlockClose.
	self assert: tokenizer getToken isBlockOpen.
	token _ tokenizer getToken.
	self assert: (token word = 'colortbl').
	token _ tokenizer getToken.
	self assert: (token string = ';').
	token _ tokenizer getToken.
	self assert: (token word = 'red');
		assert: (token arg = 255).
	token _ tokenizer getToken.
	self assert: (token word = 'green');
		assert: (token arg = 255).
	token _ tokenizer getToken.
	self assert: (token word = 'blue');
		assert: (token arg = 255).
	token _ tokenizer getToken.
	self assert: (token string = ';').
	self assert: tokenizer getToken isBlockClose.
	token _ tokenizer getToken.
	self assert: (token word = 'paperw');
		assert: (token arg = 11900).
	token _ tokenizer getToken.
	self assert: (token word = 'paperh');
		assert: (token arg = 16840).
	token _ tokenizer getToken.
	self assert: (token word = 'margl');
		assert: (token arg = 1440).
	token _ tokenizer getToken.
	self assert: (token word = 'margr');
		assert: (token arg = 1440).
	token _ tokenizer getToken.
	self assert: (token word = 'vieww');
		assert: (token arg = 9000).
	token _ tokenizer getToken.
	self assert: (token word = 'viewh');
		assert: (token arg = 8400).
	token _ tokenizer getToken.
	self assert: (token word = 'viewkind');
		assert: (token arg = 0).
	self assert: tokenizer getToken isBlockClose.
	self deny: tokenizer moreTokens.
! !

!RTFTokenizerTest methodsFor: 'as yet unclassified' stamp: 'MR 5/25/2006 18:38'!
testMacLookAhead
	| string tokenizer |

	"string is an empty rtf generated by word 2003"	
	string _ '{\rtf1\mac\ansicpg10000\cocoartf824\cocoasubrtf350
{\fonttbl}
{\colortbl;\red255\green255\blue255;}
\paperw11900\paperh16840\margl1440\margr1440\vieww9000\viewh8400\viewkind0
}'.

	tokenizer _ RTFTokenizer newFromString: string.

	self
		assert: ((tokenizer lookAhead: 0) = nil);
		assert: (tokenizer lookAhead: 1) isBlockOpen;
		assert: ((tokenizer lookAhead: 2) word = 'rtf');
		assert: ((tokenizer lookAhead: 2) arg = 1);
		assert: ((tokenizer lookAhead: 3) word = 'mac');
		assert: (tokenizer moreTokens).

! !

!RTFTokenizerTest methodsFor: 'as yet unclassified' stamp: 'MR 5/25/2006 18:45'!
testWinGetToken
	| string tokenizer token |

	"string is an empty rtf generated by word 2003"	
	string _ '{\rtf1\ansi\ansicpg1252\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1033\deflangfe1033{\fonttbl{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}{\f37\froman\fcharset238\fprq2 Times New Roman CE;}
{\f38\froman\fcharset204\fprq2 Times New Roman Cyr;}{\f40\froman\fcharset161\fprq2 Times New Roman Greek;}{\f41\froman\fcharset162\fprq2 Times New Roman Tur;}{\f42\froman\fcharset177\fprq2 Times New Roman (Hebrew);}
{\f43\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f44\froman\fcharset186\fprq2 Times New Roman Baltic;}{\f45\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;
\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;
\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}{\*\cs10 \additive \ssemihidden 
Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\trcbpat1\trcfpat1\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\latentstyles\lsdstimax156\lsdlockeddef0}{\*\rsidtbl \rsid1642803}{\*\generator Micro
soft Word 11.0.6568;}{\info{\author Compay Sedungo}{\operator Compay Sedungo}{\creatim\yr2006\mo5\dy24\hr13\min40}{\revtim\yr2006\mo5\dy24\hr13\min40}{\version1}{\edmins0}{\nofpages1}{\nofwords0}{\nofchars0}{\*\company WAY AHEAD}{\nofcharsws0}
{\vern24579}}\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1800\dgvorigin1440\dghshow1\dgvshow1
\jexpand\viewkind1\viewscale75\viewzk1\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel
\wrppunct\asianbrkrule\rsidroot1642803\newtblstyruls\nogrowautofit \fet0\sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang 
{\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang 
{\pntxtb (}{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid1642803 
\par }}'.

	tokenizer _ RTFTokenizer newFromString: string.

	"start parsing check"
	self assert: tokenizer getToken isBlockOpen.
	token _ tokenizer getToken.
	self assert: (token word = 'rtf');
		assert: (token arg = 1).
	token _ tokenizer getToken.
	self assert: (token word = 'ansi').
	1 to: 9 do: [:i | tokenizer getToken].
	self assert: tokenizer getToken isBlockOpen.
	1 to: 7 do: [:i | tokenizer getToken].
	token _ tokenizer getToken.
	self assert: (token word = '*').
	tokenizer getToken.
	token _ tokenizer getToken.
	self assert: (token string = '02020603050405020304').
	token _ tokenizer getToken.
	self assert: token isBlockClose.
	token _ tokenizer getToken.
	self assert: (token string = 'Times New Roman;').
	token _ tokenizer getToken.
	self assert: token isBlockClose.
	token _ tokenizer getToken.
	self assert: token isBlockOpen.

	"end parsing check"
	1 to: 464 do: [:i | tokenizer getToken].
	token _ tokenizer getToken.
	self assert: (token word = 'par').
	token _ tokenizer getToken.
	self assert: token isBlockClose.
	token _ tokenizer getToken.
	self assert: token isBlockClose.
	self deny: tokenizer moreTokens.
	
! !

!RTFTokenizerTest methodsFor: 'as yet unclassified' stamp: 'MR 5/24/2006 14:50'!
testWinLookAhead
	| string tokenizer |

	"string is an empty rtf generated by word 2003"	
	string _ '{\rtf1\ansi\ansicpg1252\uc1\deff0\stshfdbch0\stshfloch0\stshfhich0\stshfbi0\deflang1033\deflangfe1033{\fonttbl{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}{\f37\froman\fcharset238\fprq2 Times New Roman CE;}
{\f38\froman\fcharset204\fprq2 Times New Roman Cyr;}{\f40\froman\fcharset161\fprq2 Times New Roman Greek;}{\f41\froman\fcharset162\fprq2 Times New Roman Tur;}{\f42\froman\fcharset177\fprq2 Times New Roman (Hebrew);}
{\f43\froman\fcharset178\fprq2 Times New Roman (Arabic);}{\f44\froman\fcharset186\fprq2 Times New Roman Baltic;}{\f45\froman\fcharset163\fprq2 Times New Roman (Vietnamese);}}{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;
\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;
\red128\green128\blue128;\red192\green192\blue192;}{\stylesheet{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}{\*\cs10 \additive \ssemihidden 
Default Paragraph Font;}{\*\ts11\tsrowd\trftsWidthB3\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3\trcbpat1\trcfpat1\tscellwidthfts0\tsvertalt\tsbrdrt\tsbrdrl\tsbrdrb\tsbrdrr\tsbrdrdgl\tsbrdrdgr\tsbrdrh\tsbrdrv 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1024\langfe1024\cgrid\langnp1024\langfenp1024 \snext11 \ssemihidden Normal Table;}}{\*\latentstyles\lsdstimax156\lsdlockeddef0}{\*\rsidtbl \rsid1642803}{\*\generator Micro
soft Word 11.0.6568;}{\info{\author Compay Sedungo}{\operator Compay Sedungo}{\creatim\yr2006\mo5\dy24\hr13\min40}{\revtim\yr2006\mo5\dy24\hr13\min40}{\version1}{\edmins0}{\nofpages1}{\nofwords0}{\nofchars0}{\*\company WAY AHEAD}{\nofcharsws0}
{\vern24579}}\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1800\dgvorigin1440\dghshow1\dgvshow1
\jexpand\viewkind1\viewscale75\viewzk1\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule\nobrkwrptbl\snaptogridincell\allowfieldendsel
\wrppunct\asianbrkrule\rsidroot1642803\newtblstyruls\nogrowautofit \fet0\sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl\sftnbj {\*\pnseclvl1\pnucrm\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang 
{\pntxta .}}{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang {\pntxta .}}{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang {\pntxta )}}{\*\pnseclvl5\pndec\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang 
{\pntxtb (}{\pntxta )}}{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}\pard\plain 
\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\insrsid1642803 
\par }}'.

	tokenizer _ RTFTokenizer newFromString: string.

	self
		assert: ((tokenizer lookAhead: 0) = nil);
		assert: (tokenizer lookAhead: 1) isBlockOpen;
		assert: ((tokenizer lookAhead: 2) word = 'rtf');
		assert: ((tokenizer lookAhead: 2) arg = 1);
		assert: ((tokenizer lookAhead: 3) word = 'ansi');
		assert: (tokenizer moreTokens).

! !
