'From Cuis 5.0 of 7 November 2016 [latest update: #3253] on 15 January 2018 at 8:40:09 am'!
'Description Various code used in Squeak which is not in Cuis.'!
!provides: 'SqueakCompatibility' 1 28!
!requires: 'Cuis-Base' 42 2658 nil!
SystemOrganization addCategory: #SqueakCompatibility!


!classDefinition: #KlattResonatorIndices category: #SqueakCompatibility!
SharedPool subclass: #KlattResonatorIndices
	instanceVariableNames: ''
	classVariableNames: 'R1c R1vp R2c R2fp R2vp R3c R3fp R3vp R4c R4fp R4vp R5c R5fp R6c R6fp R7c R8c Rnpc Rnpp Rnz Rout Rtpc Rtpp Rtz'
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'KlattResonatorIndices class' category: #SqueakCompatibility!
KlattResonatorIndices class
	instanceVariableNames: ''!

!classDefinition: #StringHolder category: #SqueakCompatibility!
TextModel subclass: #StringHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'StringHolder class' category: #SqueakCompatibility!
StringHolder class
	instanceVariableNames: ''!

!classDefinition: #Model category: #SqueakCompatibility!
ActiveModel subclass: #Model
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'Model class' category: #SqueakCompatibility!
Model class
	instanceVariableNames: ''!

!classDefinition: #TimeStamp category: #SqueakCompatibility!
DateAndTime subclass: #TimeStamp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TimeStamp class' category: #SqueakCompatibility!
TimeStamp class
	instanceVariableNames: ''!

!classDefinition: #ByteString category: #SqueakCompatibility!
String variableByteSubclass: #ByteString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ByteString class' category: #SqueakCompatibility!
ByteString class
	instanceVariableNames: ''!

!classDefinition: #MultiByteFileStream category: #SqueakCompatibility!
StandardFileStream subclass: #MultiByteFileStream
	instanceVariableNames: 'lineEndConvention'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'MultiByteFileStream class' category: #SqueakCompatibility!
MultiByteFileStream class
	instanceVariableNames: ''!

!classDefinition: #CrLfFileStream category: #SqueakCompatibility!
MultiByteFileStream subclass: #CrLfFileStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'CrLfFileStream class' category: #SqueakCompatibility!
CrLfFileStream class
	instanceVariableNames: ''!

!classDefinition: #InvalidDirectoryError category: #SqueakCompatibility!
Error subclass: #InvalidDirectoryError
	instanceVariableNames: 'pathName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'InvalidDirectoryError class' category: #SqueakCompatibility!
InvalidDirectoryError class
	instanceVariableNames: ''!

!classDefinition: #ProvideAnswerNotification category: #SqueakCompatibility!
Notification subclass: #ProvideAnswerNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ProvideAnswerNotification class' category: #SqueakCompatibility!
ProvideAnswerNotification class
	instanceVariableNames: ''!

!classDefinition: #ClassTestCase category: #SqueakCompatibility!
TestCase subclass: #ClassTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ClassTestCase class' category: #SqueakCompatibility!
ClassTestCase class
	instanceVariableNames: ''!

!classDefinition: #TestObjectsAsMethods category: #SqueakCompatibility!
TestCase subclass: #TestObjectsAsMethods
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TestObjectsAsMethods class' category: #SqueakCompatibility!
TestObjectsAsMethods class
	instanceVariableNames: ''!

!classDefinition: #FileDirectory category: #SqueakCompatibility!
Object subclass: #FileDirectory
	instanceVariableNames: 'pathName'
	classVariableNames: 'CurrentDirectory DirectoryClass ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'FileDirectory class' category: #SqueakCompatibility!
FileDirectory class
	instanceVariableNames: ''!

!classDefinition: #AcornFileDirectory category: #SqueakCompatibility!
FileDirectory subclass: #AcornFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'AcornFileDirectory class' category: #SqueakCompatibility!
AcornFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #DosFileDirectory category: #SqueakCompatibility!
FileDirectory subclass: #DosFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'DosFileDirectory class' category: #SqueakCompatibility!
DosFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #UnixFileDirectory category: #SqueakCompatibility!
FileDirectory subclass: #UnixFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'UnixFileDirectory class' category: #SqueakCompatibility!
UnixFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #SmalltalkImage category: #SqueakCompatibility!
Object subclass: #SmalltalkImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'SmalltalkImage class' category: #SqueakCompatibility!
SmalltalkImage class
	instanceVariableNames: ''!

!classDefinition: #SqDirectoryEntry category: #SqueakCompatibility!
Object subclass: #SqDirectoryEntry
	instanceVariableNames: 'directory name creationTime modificationTime fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'SqDirectoryEntry class' category: #SqueakCompatibility!
SqDirectoryEntry class
	instanceVariableNames: ''!

!classDefinition: #DirectoryEntryDirectory category: #SqueakCompatibility!
SqDirectoryEntry subclass: #DirectoryEntryDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'DirectoryEntryDirectory class' category: #SqueakCompatibility!
DirectoryEntryDirectory class
	instanceVariableNames: ''!

!classDefinition: #DirectoryEntryFile category: #SqueakCompatibility!
SqDirectoryEntry subclass: #DirectoryEntryFile
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'DirectoryEntryFile class' category: #SqueakCompatibility!
DirectoryEntryFile class
	instanceVariableNames: ''!

!classDefinition: #UnsupportedInCuis category: #SqueakCompatibility!
Object subclass: #UnsupportedInCuis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'UnsupportedInCuis class' category: #SqueakCompatibility!
UnsupportedInCuis class
	instanceVariableNames: ''!

!classDefinition: #AlignmentMorph category: #SqueakCompatibility!
UnsupportedInCuis subclass: #AlignmentMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'AlignmentMorph class' category: #SqueakCompatibility!
AlignmentMorph class
	instanceVariableNames: ''!

!classDefinition: #BorderStyle category: #SqueakCompatibility!
UnsupportedInCuis subclass: #BorderStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'BorderStyle class' category: #SqueakCompatibility!
BorderStyle class
	instanceVariableNames: ''!

!classDefinition: #ComponentInstance category: #SqueakCompatibility!
UnsupportedInCuis subclass: #ComponentInstance
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ComponentInstance class' category: #SqueakCompatibility!
ComponentInstance class
	instanceVariableNames: ''!

!classDefinition: #LayoutFrame category: #SqueakCompatibility!
UnsupportedInCuis subclass: #LayoutFrame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'LayoutFrame class' category: #SqueakCompatibility!
LayoutFrame class
	instanceVariableNames: ''!

!classDefinition: #MCWorkingCopy category: #SqueakCompatibility!
UnsupportedInCuis subclass: #MCWorkingCopy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'MCWorkingCopy class' category: #SqueakCompatibility!
MCWorkingCopy class
	instanceVariableNames: ''!

!classDefinition: #PackageOrganizer category: #SqueakCompatibility!
UnsupportedInCuis subclass: #PackageOrganizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'PackageOrganizer class' category: #SqueakCompatibility!
PackageOrganizer class
	instanceVariableNames: ''!

!classDefinition: #PluggableTextMorph category: #SqueakCompatibility!
UnsupportedInCuis subclass: #PluggableTextMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'PluggableTextMorph class' category: #SqueakCompatibility!
PluggableTextMorph class
	instanceVariableNames: ''!

!classDefinition: #ProportionalLayout category: #SqueakCompatibility!
UnsupportedInCuis subclass: #ProportionalLayout
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ProportionalLayout class' category: #SqueakCompatibility!
ProportionalLayout class
	instanceVariableNames: ''!

!classDefinition: #SimpleButtonMorph category: #SqueakCompatibility!
UnsupportedInCuis subclass: #SimpleButtonMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'SimpleButtonMorph class' category: #SqueakCompatibility!
SimpleButtonMorph class
	instanceVariableNames: ''!

!classDefinition: #TableLayout category: #SqueakCompatibility!
UnsupportedInCuis subclass: #TableLayout
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TableLayout class' category: #SqueakCompatibility!
TableLayout class
	instanceVariableNames: ''!

!classDefinition: #TextMorph category: #SqueakCompatibility!
UnsupportedInCuis subclass: #TextMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TextMorph class' category: #SqueakCompatibility!
TextMorph class
	instanceVariableNames: ''!

!classDefinition: #TextStyle category: #SqueakCompatibility!
UnsupportedInCuis subclass: #TextStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TextStyle class' category: #SqueakCompatibility!
TextStyle class
	instanceVariableNames: ''!

!classDefinition: #TranscriptStream category: #SqueakCompatibility!
UnsupportedInCuis subclass: #TranscriptStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TranscriptStream class' category: #SqueakCompatibility!
TranscriptStream class
	instanceVariableNames: ''!

!classDefinition: #TransferMorph category: #SqueakCompatibility!
UnsupportedInCuis subclass: #TransferMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TransferMorph class' category: #SqueakCompatibility!
TransferMorph class
	instanceVariableNames: ''!

!classDefinition: #UIManager category: #SqueakCompatibility!
UnsupportedInCuis subclass: #UIManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'UIManager class' category: #SqueakCompatibility!
UIManager class
	instanceVariableNames: ''!


!Model commentStamp: '<historical>' prior: 0!
Compatibility. Prefer ActiveModel.!

!MultiByteFileStream commentStamp: 'jmv 5/8/2015 15:03' prior: 0!
Not really a MultiByteXXX. Accept #lineEndConvention and honor it, on Write. On Read, do no conversions.!

!CrLfFileStream commentStamp: '<historical>' prior: 0!
Do line ending conversion on write. By default, write files with host platform convention.!

!FileDirectory commentStamp: '<historical>' prior: 0!
A FileDirectory represents a folder or directory in the underlying platform's file system. It carries a fully-qualified path name for the directory it represents, and can enumerate the files and directories within that directory.

A FileDirectory can be thought of as a Dictionary whose keys are the local names of files in that directory, and whose values are directory "entries". Each entry is an array of five items:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

The times are given in seconds, and can be converted to a time and date via Time>dateAndTimeFromSeconds:. See the comment in lookupEntry:... which provides primitive access to this information.
!

!DosFileDirectory commentStamp: '<historical>' prior: 0!
I represent a DOS or Windows FileDirectory.
!

!UnixFileDirectory commentStamp: '<historical>' prior: 0!
I represent a Unix FileDirectory.
!

!SqDirectoryEntry commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to either a file or a directory.!

!DirectoryEntryDirectory commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to a directory.!

!DirectoryEntryFile commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to a file.!

!KlattResonatorIndices class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 20:17'!
initialize
	"KlattResonatorIndices initialize"
	Rnpp := 1.
	Rtpp := 2.
	R1vp := 3.
	R2vp := 4.
	R3vp := 5.
	R4vp := 6.
	R2fp := 7.
	R3fp := 8.
	R4fp := 9.
	R5fp := 10.
	R6fp := 11.
	R1c := 12.
	R2c := 13.
	R3c := 14.
	R4c := 15.
	R5c := 16.
	R6c := 17.
	R7c := 18.
	R8c := 19.
	Rnpc := 20.
	Rnz := 21.
	Rtpc := 22.
	Rtz := 23.
	Rout := 24.! !

!KlattResonatorIndices class methodsFor: 'frame parameter data' stamp: 'dtl 11/15/2009 10:30'!
parameterData
	"This is a table describing the Klatt parameters. The columns are: parameter name, minimum value, maximum, parameter description, unit."
	^ #(
	"Excitation source (voice, aspiration and friction):"
		(f0 20 1000 'Fundamental frequency (hz)' hz)
		(flutter 0 1 'Amount of flutter' value)
		(jitter 0 1 'Amount of jitter' value)
		(shimmer 0 1 'Amount of shimmer' value)
		(diplophonia 0 1 'Amount of diplophonia' value)
		(voicing 0 80 'Amplitude of voicing' hz)
		(ro 0.01 1 'Relative duration of open phase of voicing waveform = Te/T0 (0.01 - 1)' value)
		(ra 0.01 0.2 'Relative duration of return phase of voicing waveform = Ta/T0 (0.01 - 1)' value)
		(rk 0.01 1 'Simmetry of the glottal pulse = (Te-Tp)/Tp (0.01 - 1)' value)
		(aspiration 0 80 'Amplitude of aspiration' dB)
		(friction 0 80 'Amplitude of friction' dB)
		(turbulence 0 80 'Amplitude of turbulence (in open glottal phase)' dB)

	"Formants frequencies and bandwidths:"	
		(f1 200 1300 'Frequency of 1st formant' hz)
		(b1 40 1000 'Bandwidth of 1st formant' hz)
		(df1 0 100 'Change in F1 during open portion of period' hz)
		(db1 0 400 'Change in B1 during open portion of period' hz)
		(f2 550 3000 'Frequency of 2nd formant' hz)
		(b2 40 1000 'Bandwidth of 2nd formant' hz)
		(f3 1200 4999 'Frequency of 3rd formant' hz)
		(b3 40 1000 'Bandwidth of 3rd formant' hz)
		(f4 1200 4999 'Frequency of 4th formant' hz)
		(b4 40 1000 'Bandwidth of 4th formant' hz)
		(f5 1200 4999 'Frequency of 5th formant' hz)
		(b5 40 1000 'Bandwidth of 5th formant' hz)
		(f6 1200 4999 'Frequency of 6th formant' hz)
		(b6 40 1000 'Bandwidth of 6th formant' hz)
		(fnp 248 528 'Frequency of nasal pole' hz)
		(bnp 40 1000 'Bandwidth of nasal pole' hz)
		(fnz 248 528 'Frequency of nasal zero' hz)
		(bnz 40 1000 'Bandwidth of nasal zero' hz)
		(ftp 300 3000 'Frequency of tracheal pole' hz)
		(btp 40 1000 'Bandwidth of tracheal pole' hz)
		(ftz 300 3000 'Frequency of tracheal zero' hz)
		(btz 40 2000 'Bandwidth of tracheal zero' hz)

	"Parallel Friction-Excited:"
		(a2f 0 80 'Amplitude of friction-excited parallel 2nd formant' dB)
		(a3f 0 80 'Amplitude of friction-excited parallel 3rd formant' dB)
		(a4f 0 80 'Amplitude of friction-excited parallel 4th formant' dB)
		(a5f 0 80 'Amplitude of friction-excited parallel 5th formant' dB)
		(a6f 0 80 'Amplitude of friction-excited parallel 6th formant' dB)
		(bypass 0 80 'Amplitude of friction-excited parallel bypass path' dB)
		(b2f 40 1000 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b3f 60 1000 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b4f 100 1000 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b5f 100 1500 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b6f 100 4000 'Bandwidth of friction-excited parallel 2nd formant' hz)

	"Parallel Voice-Excited:"
		(anv 0 80 'Amplitude of voice-excited parallel nasal formant' dB)
		(a1v 0 80 'Amplitude of voice-excited parallel 1st formant' dB)
		(a2v 0 80 'Amplitude of voice-excited parallel 2nd formant' dB)
		(a3v 0 80 'Amplitude of voice-excited parallel 3rd formant' dB)
		(a4v 0 80 'Amplitude of voice-excited parallel 4th formant' dB)
		(atv 0 80 'Amplitude of voice-excited parallel tracheal formant' dB)

	"Overall gain:"
		(gain 0 80 'Overall gain' dB))! !

!KlattResonatorIndices class methodsFor: 'frame parameter data' stamp: 'dtl 11/15/2009 10:31'!
parameterNames
	^ self parameterData collect: [ :each | each first]! !

!MultiByteFileStream methodsFor: 'access' stamp: 'jmv 5/8/2015 15:12'!
lineEndConvention: aSymbol

	lineEndConvention _ aSymbol caseOf: {
		[ #lf ] -> [ String lfString ].
		[ #cr ] -> [ String crString ] }.! !

!MultiByteFileStream methodsFor: 'line end conversion' stamp: 'jmv 5/8/2015 15:17'!
nextPut: char
	(char isLineSeparator and: [ lineEndConvention notNil ])
		ifTrue: [self nextPutAll: lineEndConvention ]
		ifFalse: [super nextPut: char ].
	^char! !

!MultiByteFileStream methodsFor: 'line end conversion' stamp: 'jmv 5/8/2015 15:17'!
nextPutAll: aString
	| converted |
	converted _ lineEndConvention
		ifNil: [ aString ]
		ifNotNil: [ aString withLineEndings: lineEndConvention ].
	.super nextPutAll: converted.
	^aString! !

!CrLfFileStream methodsFor: 'initialization' stamp: 'jmv 5/8/2015 15:28'!
initialize
	"By default, use host platform convention"
	lineEndConvention _ FileDirectory pathNameDelimiter = $\
		ifFalse: [ String lfString ]
		ifTrue: [ String crlfString ]! !

!InvalidDirectoryError methodsFor: 'exceptionDescription' stamp: 'ar 5/30/2001 20:49'!
defaultAction
	"Return an empty list as the default action of signaling the occurance of an invalid directory."
	^#()! !

!InvalidDirectoryError methodsFor: 'accessing' stamp: 'ar 5/30/2001 20:44'!
pathName
	^pathName! !

!InvalidDirectoryError methodsFor: 'accessing' stamp: 'ar 5/30/2001 20:45'!
pathName: badPathName
	pathName _ badPathName! !

!InvalidDirectoryError class methodsFor: 'exceptionInstantiator' stamp: 'ar 5/30/2001 20:49'!
pathName: badPathName
	^self new pathName: badPathName! !

!FileDirectory methodsFor: 'comparing' stamp: 'jmv 1/11/2015 21:03'!
= aDirectory

	"Any object is equal to itself"
	self == aDirectory ifTrue: [ ^ true ].
	(aDirectory is: #FileDirectory) ifFalse: [ ^false ].

	"Compare two FileDirectory instances."
	^ (pathName asString 
			compare: aDirectory pathName asString 
			caseSensitive: (self isCaseSensitive | aDirectory isCaseSensitive)) = 2! !

!FileDirectory methodsFor: 'comparing' stamp: 'cwp 11/16/2009 22:10'!
hash
	"Hash is reimplemented because #= is reimplemented"
	^pathName asString asLowercase hash! !

!FileDirectory methodsFor: 'file directory' stamp: 'bf 9/5/2010 00:02'!
assureExistence
	"Make sure the current directory exists. If necessary, create all parts in between"

	self exists ifFalse: [
 		self containingDirectory
			assureExistence;
			createDirectory: self localName]! !

!FileDirectory methodsFor: 'file directory' stamp: 'RAA 7/28/2000 13:47'!
localNameFor: fullName
	"Return the local part the given name."

	^self class localNameFor: fullName! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jm 5/8/1998 20:48'!
checkName: aFileName fixErrors: fixing
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."

	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength _ self class maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixing
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].

	^ aFileName
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 6/17/2015 11:23'!
fileNamesMatching: pat
	"FileDirectory smalltalkImageDirectory fileNamesMatching: '*'"

	^ self fileNames select: [:name | pat match: name]
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 4/19/2015 08:35'!
fullNameFor: fileName
	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."
	"Details: Note that path relative to a directory, such as '../../foo' are disallowed by this algorithm.  Also note that this method is tolerent of a nil argument -- is simply returns nil in this case."

	| correctedLocalName prefix |
	fileName ifNil: [^ nil].
	self class activeDirectoryClass splitName: fileName to:
		[:filePath :localName |
			correctedLocalName _ localName isEmpty 
				ifFalse: [self checkName: localName fixErrors: true]
				ifTrue: [localName].
			prefix _ self fullPathFor: filePath].
	prefix isEmpty
		ifTrue: [^correctedLocalName].
	prefix last = self pathNameDelimiter
		ifTrue:[^ prefix, correctedLocalName]
		ifFalse:[^ prefix, self slash, correctedLocalName]! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jm 12/4/97 21:19'!
isLegalFileName: aString 
	"Answer true if the given string is a legal file name."

	^ (self checkName: aString fixErrors: true) = aString
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 12/6/2011 13:59'!
lastNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	| files splits |

	files _ self fileNamesMatching: (baseFileName,'*', self class dot, extension).
	splits _ files collect: [:file | self splitNameVersionExtensionFor: file].
	splits _ splits asArray sort: [:a :b | (a at: 2) < (b at: 2)].
	^splits isEmpty 
			ifTrue: [nil]
			ifFalse: [((splits last at: 1), '.', (splits last at: 2) asString, self class dot, extension) asFileName]! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 12/6/2011 11:01'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version |

	files _ self fileNamesMatching: (baseFileName,'*', self class dot, extension).
	splits _ files collect: [:file | self splitNameVersionExtensionFor: file].
	splits _ splits asArray sort: [:a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [version _ 1]
			ifFalse: [version _ (splits last at: 2) + 1].
	^ (baseFileName, fileNameCoda, '.', version asString, self class dot, extension) asFileName! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 12/6/2011 10:39'!
nextNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	^self nextNameFor: baseFileName coda: '' extension: extension! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'nk 12/13/2002 10:07'!
relativeNameFor: aFileName
	"Return the full name for aFileName, assuming that aFileName is a name relative to me."
	aFileName isEmpty ifTrue: [ ^pathName ].
	^aFileName first = self pathNameDelimiter
		ifTrue: [ pathName, aFileName ]
		ifFalse: [ pathName, self slash, aFileName ]
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'djp 10/27/1999 08:58'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in nextSequentialNameFor: for more details"

	| baseName version extension i j |

	baseName _ self class baseNameFor: fileName.
	extension _ self class extensionFor: fileName.
	i _ j _ baseName findLast: [:c | c isDigit not].
	i = 0
		ifTrue: [version _ 0]
		ifFalse:
			[(baseName at: i) = $.
				ifTrue:
					[version _ (baseName copyFrom: i+1 to: baseName size) asNumber.
					j _ j - 1]
				ifFalse: [version _ 0].
			baseName _ baseName copyFrom: 1 to: j].
	^ Array with: baseName with: version with: extension! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 1/5/98 20:49'!
containingDirectory
	"Return the directory containing this directory."

	^ FileDirectory on: (FileDirectory dirPathFor: pathName)
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'nk 2/23/2001 11:35'!
directoryEntry
	^self containingDirectory entryAt: self localName! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 4/19/2015 08:34'!
directoryEntryFor: filenameOrPath
	"Answer the directory entry for the given file or path. Sorta like a poor man's stat()."
	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:[:filePath :name |
		fName _ name.
		filePath isEmpty
			ifTrue: [dir _ self]
			ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir entries detect:[:entry| entry name = fName] ifNone: nil]
		ifFalse:[^dir entries detect:[:entry| entry name sameAs: fName] ifNone: nil]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 12/5/97 15:46'!
directoryNamed: localFileName
	"Return the subdirectory of this directory with the given name."

	^ FileDirectory on: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
directoryNames
	"Return a collection of names for the subdirectories of this directory."
	"FileDirectory smalltalkImageDirectory directoryNames"
	^ (self entries select: [ :entry |
		entry isDirectory ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'dtl 1/14/2018 15:26:38'!
entries
	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries index done entryArray |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	[ done ] whileFalse: [
		entryArray _ self primLookupEntryIn: pathName index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ (InvalidDirectoryError pathName: pathName) signal ].
		entryArray
			ifNil: [ done _ true ]
			ifNotNil: [ entries
				addLast: ( (entryArray at: 4)
					ifTrue: [DirectoryEntry pathName: self pathName, self slash, entryArray first]
					ifFalse: [FileEntry pathName: self pathName, self slash, entryArray first] ) ].
		index _ index + 1 ].

	^ entries asArray! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
fileAndDirectoryNames
	"FileDirectory smalltalkImageDirectory fileAndDirectoryNames"

	^ self entries collect: [:entry | entry name]
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:23'!
fileNames
	"Return a collection of names for the files (but not directories) in this directory."
	"FileDirectory smalltalkImageDirectory fileNames"
	^ (self entries select: [ :entry |
		entry isDirectory not ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'ar 3/15/2001 23:20'!
fullName
	"Return the full name of this directory."

	^pathName! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 9/27/1998 21:34'!
fullNamesOfAllFilesInSubtree
	"Answer a collection containing the full names of all the files in the subtree of the file system whose root is this directory."

	| result todo dir |
	result _ OrderedCollection new: 100.
	todo _ OrderedCollection with: self.
	[todo size > 0] whileTrue: [
		dir _ todo removeFirst.
		dir fileNames do: [:n | result add: (dir fullNameFor: n)].
		dir directoryNames do: [:n | todo add: (dir directoryNamed: n)]].
	^ result asArray
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 12/5/97 15:39'!
keysDo: nameBlock
	"Evaluate the given block for each file or directory name in this directory."

	^ self fileAndDirectoryNames do: nameBlock
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'ar 2/6/2001 15:48'!
localName
	"Return the local name of this directory."

	^FileDirectory localNameFor: pathName! !

!FileDirectory methodsFor: 'file operations' stamp: 'MPH 10/15/2000 12:43'!
copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer _ String new: 50000.
	[fileStream1 atEnd] whileFalse:
		[fileStream2 nextPutAll: (fileStream1 nextInto: buffer)].
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:21'!
copyFileNamed: fileName1 toFileNamed: fileName2
	"Copy the contents of the existing file with the first name into a new file with the second name. Both files are assumed to be in this directory."
	"FileDirectory smalltalkImageDirectory copyFileNamed: 'todo.txt' toFileNamed: 'todocopy.txt'"

	| file1 file2 |
	file1 _ (self readOnlyFileNamed: fileName1) binary.
	file2 _ (self newFileNamed: fileName2) binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:21'!
copyFileWithoutOverwriteConfirmationNamed: fileName1 toFileNamed: fileName2
	"Copy the contents of the existing file with the first name into a file with the second name (which may or may not exist). If the second file exists, force an overwrite without confirming.  Both files are assumed to be in this directory."
	"FileDirectory smalltalkImageDirectory copyFileWithoutOverwriteConfirmationNamed: 'todo.txt' toFileNamed: 'todocopy.txt'"

	| file1 file2 |
	fileName1 = fileName2 ifTrue: [^ self].
	file1 _ (self readOnlyFileNamed: fileName1) binary.
	file2 _ (self forceNewFileNamed: fileName2) binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.! !

!FileDirectory methodsFor: 'file operations' stamp: 'jm 12/4/97 22:55'!
createDirectory: localFileName
	"Create a directory with the given name in this directory. Fail if the name is bad or if a file or directory with that name already exists."

 	self primCreateDirectory: (self fullNameFor: localFileName).
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jm 4/9/1999 18:02'!
deleteDirectory: localDirName
	"Delete the directory with the given name in this directory. Fail if the path is bad or if a directory by that name does not exist."

 	self primDeleteDirectory: (self fullNameFor: localDirName).
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 3/2/2010 09:54'!
deleteFileNamed: localFileName
	"Delete the file with the given name in this directory."

	self deleteFileNamed: localFileName ifAbsent: nil! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 3/1/2010 11:22'!
deleteFileNamed: localFileName ifAbsent: failBlock
	"Delete the file of the given name if it exists, else evaluate failBlock.
	If the first deletion attempt fails do a GC to force finalization of any lost references. ar 3/21/98 17:53"
	| fullName |
	fullName _ self fullNameFor: localFileName.
	(StandardFileStream 
		retryWithGC:[self primDeleteFileNamed: (self fullNameFor: localFileName)]
		until:[:result| result notNil]
		forFileNamed: fullName) ifNil: [
			^failBlock value].
! !

!FileDirectory methodsFor: 'file operations' stamp: 'tpr 3/26/2002 16:48'!
deleteLocalFiles
	"Delete the local files in this directory."

	self fileNames do:[:fn| self deleteFileNamed: fn ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , (self fullNameFor: fn)) signal]]
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:24'!
fileOrDirectoryExists: filenameOrPath
	"Answer true if either a file or a directory file of the given name exists. The given name may be either a full path name or a local name within this directory."
	"FileDirectory smalltalkImageDirectory fileOrDirectoryExists: Smalltalk defaultSourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to: [ :filePath :name |
		fName _ name.
		dir _ filePath isEmpty
			ifTrue: [self]
			ifFalse: [FileDirectory on: filePath]].

	^ (dir includesKey: fName) or: [ fName = '' and:[ dir entries size > 1]]! !

!FileDirectory methodsFor: 'file operations' stamp: 'MPH 10/23/2000 13:31'!
putFile: file1 named: destinationFileName
	"Copy the contents of the existing fileStream into the file destinationFileName in this directory.  fileStream can be anywhere in the fileSystem."

	| file2 |
	file1 binary.
	(file2 _ self newFileNamed: destinationFileName) ifNil: [^ false].
	file2 binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.
	^ true
! !

!FileDirectory methodsFor: 'file operations' stamp: 'tk 2/26/2000 12:54'!
putFile: file1 named: destinationFileName retry: aBool
	"Copy the contents of the existing fileStream into the file destinationFileName in this directory.  fileStream can be anywhere in the fileSystem.  No retrying for local file systems."

	^ self putFile: file1 named: destinationFileName
! !

!FileDirectory methodsFor: 'file operations' stamp: 'tpr 3/26/2002 18:09'!
recursiveDelete
	"Delete the this directory, recursing down its tree."
	self directoryNames
		do: [:dn | (self directoryNamed: dn) recursiveDelete].
	self deleteLocalFiles.
	"should really be some exception handling for directory deletion, but no 
	support for it yet"
	self containingDirectory deleteDirectory: self localName! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 3/2/2010 17:02'!
rename: oldFileName toBe: newFileName
	| selection oldName newName |
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name."
	"Modified for retry after GC ar 3/21/98 18:09"
	oldName _ self fullNameFor: oldFileName.
	newName _ self fullNameFor: newFileName.
	(StandardFileStream 
		retryWithGC:[self primRename: oldName to: newName]
		until:[:result| result notNil]
		forFileNamed: oldName) ifNotNil: [ ^self].
	(self fileExists: oldFileName) ifFalse: [
		^self error:'Attempt to rename a non-existent file'.
	].
	(self fileExists: newFileName) ifTrue:[
		selection _ (PopUpMenu labels:
'delete old version
cancel')
				startUpWithCaption: 'Trying to rename a file to be
', newFileName , '
and it already exists.'.
		selection = 1 ifTrue: [
			self deleteFileNamed: newFileName.
			^ self rename: oldFileName toBe: newFileName]].
	^self error:'Failed to rename file'.! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:21'!
directoryExists: filenameOrPath
	"Answer true if a directory of the given name exists. The given name may be either a full path name or a local directory within this directory."
	"FileDirectory smalltalkImageDirectory directoryExists: FileDirectory smalltalkImageDirectory pathName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ self directoryNamed: filePath]].

	^dir exists and: [
		self isCaseSensitive 
			ifTrue:[dir directoryNames includes: fName]
			ifFalse:[dir directoryNames anySatisfy: [:name| name sameAs: fName]]].
! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 8/28/2011 16:13'!
exists
"Answer whether the directory exists"

	| result |
	result _ self primLookupEntryIn: pathName index: 1.
	^ result ~~ #badDirectoryPath! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:23'!
fileExists: filenameOrPath
	"Answer true if a file of the given name exists. The given name may be either a full path name or a local file within this directory."
	"FileDirectory smalltalkImageDirectory fileExists: Smalltalk defaultSourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir fileNames includes: fName]
		ifFalse:[^dir fileNames anySatisfy: [:name| name sameAs: fName]].	! !

!FileDirectory methodsFor: 'testing' stamp: 'di 11/21/1999 20:17'!
includesKey: localName
	"Answer true if this directory includes a file or directory of the given name. Note that the name should be a local file name, in contrast with fileExists:, which takes either local or full-qualified file names."
	"(FileDirectory on: Smalltalk vmPath) includesKey: 'SqueakV2.sources'"
	self isCaseSensitive
		ifTrue:[^ self fileAndDirectoryNames includes: localName]
		ifFalse:[^ self fileAndDirectoryNames anySatisfy: [:str| str sameAs: localName]].! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:41'!
is: aSymbol
	^ aSymbol == #FileDirectory or: [ super is: aSymbol ]! !

!FileDirectory methodsFor: 'testing' stamp: 'ar 5/30/2001 21:42'!
isAFileNamed: fName
	^FileStream isAFileNamed: (self fullNameFor: fName)! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 3/16/2010 19:23'!
isCaseSensitive
	"Return true if file names are treated case sensitive"
	"Warning: This method can't always be trusted, see comment in other implementors"
	^self class isCaseSensitive! !

!FileDirectory methodsFor: 'file status' stamp: 'mdr 1/14/2000 21:16'!
entryAt: fileName  
	"find the entry with local name fileName"

	^self entryAt: fileName ifAbsent: [ self error: 'file not in directory: ', fileName ].! !

!FileDirectory methodsFor: 'file status' stamp: 'jmv 6/11/2012 12:24'!
entryAt: fileName ifAbsent: aBlock
	"Find the entry with local name fileName and answer it.
	If not found, answer the result of evaluating aBlock."
	| comparisonBlock |
	comparisonBlock _ self isCaseSensitive
		ifTrue: [
			[ :entry |
			entry name = fileName ]]
		ifFalse: [
			[ :entry |
			entry name sameAs: fileName ]].
	^ self entries
		detect: comparisonBlock
		ifNone: aBlock! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:34'!
fileNamed: localFileName
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist."

	^ FileStream fileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:18'!
fileNamed: localFileName do: aBlock
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist.
	Evaluate aBlock, and close the file"

	(self fileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
forceNewFileNamed: localFileName
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking."

	^ FileStream forceNewFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:05'!
forceNewFileNamed: localFileName do: aBlock
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking.
	Evaluate aBlock, and close the file"

	(self forceNewFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
newFileNamed: localFileName
	"Create a new file with the given name in this directory.
	If the file already exists, give the chance to pick another name or overwrite it."

	^ FileStream newFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 5/20/2015 08:52'!
newFileNamed: localFileName do: aBlock
	"Create a new file with the given name in this directory and pass it as argument to aBlock."

	^ FileStream newFileNamed: (self fullNameFor: localFileName) do: aBlock
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
oldFileNamed: localFileName
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort."

	^ FileStream oldFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:07'!
oldFileNamed: localFileName do: aBlock
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort.
	Evaluate aBlock, and close the file"

	(self oldFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:14'!
oldFileNamed: localFileName ifExistsDo: aBlock
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, do nothing.
	If the file exists, evaluate aBlock, and close the file"

	(self oldFileOrNoneNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/17/2015 14:32'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."
	| fullName |

	"If full path name is not specified, get it assuming current directory."
	fullName _ self fullNameFor: fileName.

	^ FileStream oldFileOrNoneNamed: fullName
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
readOnlyFileNamed: localFileName
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort."

	^ FileStream readOnlyFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:11'!
readOnlyFileNamed: localFileName do: aBlock
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort.
	Evaluate aBlock, and close the file"

	(self readOnlyFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'searching' stamp: 'jmv 6/17/2015 11:24'!
filesContaining: searchString caseSensitive: aBoolean
	| aList |
	"Search the contents of all files in the receiver and its subdirectories for the search string.  Return a list of paths found.  Make the search case sensitive if aBoolean is true."

	aList _ OrderedCollection new.
	self withAllFilesDo: [ :stream |
			(stream contentsOfEntireFile includesSubstring: searchString caseSensitive: aBoolean)
				ifTrue:	[ aList add: stream name ]]
		andDirectoriesDo: [ :d | d pathName ].
	^ aList

"FileDirectory smalltalkImageDirectory filesContaining: 'includesSubstring:'  caseSensitive: true"! !

!FileDirectory methodsFor: 'searching' stamp: 'jmv 12/6/2009 22:48'!
withAllFilesDo: fileStreamBlock andDirectoriesDo: directoryBlock

	"For the receiver and all it's subdirectories evaluate directoryBlock.
	For a read only file stream on each file within the receiver 
	and it's subdirectories evaluate fileStreamBlock."

	| todo dir |

	todo := OrderedCollection with: self.
	[todo size > 0] whileTrue: [
		dir := todo removeFirst.
		directoryBlock value: dir.
		dir fileNames do: [: n |
			"For example, files inside a Mac .app file are not accessible, and the stream is nil"
			(FileStream oldFileOrNoneNamed: (dir fullNameFor: n))
				ifNotNil: [ :fileStream | fileStreamBlock value: fileStream ]].
		dir directoryNames do: [: n | 
			todo add: (dir directoryNamed: n)]]

! !

!FileDirectory methodsFor: 'path access' stamp: 'ar 12/18/1999 01:01'!
fullPathFor: path
	^path isEmpty ifTrue:[pathName] ifFalse:[path]! !

!FileDirectory methodsFor: 'path access' stamp: 'tk 5/18/1998 22:29'!
on: fullPath
	"Return another instance"

	^ self class on: fullPath! !

!FileDirectory methodsFor: 'path access' stamp: 'jm 12/5/97 12:18'!
pathName
	"Return the path from the root of the file system to this directory."

	^ pathName
! !

!FileDirectory methodsFor: 'path access' stamp: 'jm 12/5/97 12:19'!
pathNameDelimiter
	"Return the delimiter character for this kind of directory. This depends on the current platform."

	^ self class pathNameDelimiter
! !

!FileDirectory methodsFor: 'path access' stamp: 'jm 12/5/97 12:17'!
pathParts
	"Return the path from the root of the file system to this directory as an array of directory names."

	^ pathName findTokens: self pathNameDelimiter asString
! !

!FileDirectory methodsFor: 'path access' stamp: 'ar 12/18/1999 00:36'!
slash
	^self class slash! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primCreateDirectory: fullPath
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primDeleteDirectory: fullPath
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primDeleteFileNamed: aFileName
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.)

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primRename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !

!FileDirectory methodsFor: 'private' stamp: 'jm 12/4/97 22:44'!
setPathName: pathString

	pathName _ pathString.
! !

!FileDirectory methodsFor: 'printing' stamp: 'jm 12/4/97 19:41'!
printOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPutAll: self class name.
	aStream nextPutAll: ' on '.
	pathName printOn: aStream.
! !

!FileDirectory class methodsFor: 'private' stamp: 'jmv 4/19/2015 08:34'!
activeDirectoryClass
	"Return the concrete FileDirectory subclass for the platform on which we are currently running."

	DirectoryClass ifNotNil: [ ^DirectoryClass ].
	
	FileDirectory allSubclasses do: [ :cls |
		cls isActiveDirectoryClass ifTrue: [
			DirectoryClass _ cls.
			^ DirectoryClass ]].

	"no responding subclass; use FileDirectory"
	DirectoryClass _ FileDirectory.
	^DirectoryClass! !

!FileDirectory class methodsFor: 'private' stamp: 'TPR 5/10/1998 21:40'!
isActiveDirectoryClass
	"Does this class claim to be that properly active subclass of FileDirectory for this platform?
	Default test is whether the primPathNameDelimiter matches the one for this class. Other tests are possible"

	^self pathNameDelimiter = self primPathNameDelimiter
! !

!FileDirectory class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primPathNameDelimiter
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 6/17/2015 11:24'!
baseNameFor: filename
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileDirectory baseNameFor: ((FileDirectory smalltalkImageDirectory directoryNamed: 'foo.bar') fullNameFor:'blim.blam') 
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileDirectory baseNameFor: ((FileDirectory smalltalkImageDirectory directoryNamed: 'foo.bar') fullNameFor:'blim')
		should be the same and NOT  'foo'
		
		Oh, and
		FileDirectory baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	| ext |
	ext _ self extensionFor: filename.
	ext isEmpty ifTrue: [
		^filename ].
	^filename copyFrom: 1 to: filename size - ext size - 1.
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'TPR 5/10/1998 21:32'!
changeSuffix
"if 'changes' is not suitable, override this message to return something that is ok"
	^'changes'! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 6/17/2015 13:43'!
checkName: fileName fixErrors: flag
	"Check a string fileName for validity as a file name on the current default file system. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is to truncate the name to 31 chars. Subclasses can do any kind of checking and correction appropriate to the underlying platform."

"This doesn't really belong in the class. Different file systems (mounted on the same machine)
can have different ideas about this. In any case, maybe this dies when we adopt FileMan."

	^ self smalltalkImageDirectory
		checkName: fileName
		fixErrors: flag
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
dirPathFor: fullName 
	"Return the directory part the given name."
	self activeDirectoryClass
		splitName: fullName
		to: [:dirPath :localName | ^ dirPath]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
extensionFor: pathName
	"Return the extension of given file name, if any.
	FileDirectory extensionFor: 'writings.txt'
	FileDirectory extensionFor: 'optionalstuff.pck.st'
	FileDirectory extensionFor: 'optionalstuff.pck'
	FileDirectory extensionFor: 'code.cs.st'
	FileDirectory extensionFor: 'code.cs'
	"

	| delim i |
	self splitName: pathName to: [ :path :filename |
		delim _ self activeDirectoryClass extensionDelimiter.
		{ 'cs.st' . 'pck.st' } do: [ :specialExtension |
			(filename endsWith: delim asString, specialExtension)
				ifTrue: [ ^specialExtension ]].
		i _ filename findLast: [ :c | c = delim ].
		^ i = 0
			ifTrue: [ '']
			ifFalse: [ filename copyFrom: i + 1 to: filename size ]]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'mir 10/11/2000 17:38'!
fileName: fileName extension: fileExtension
	| extension |
	extension _ FileDirectory dot , fileExtension.
	^(fileName endsWith: extension)
		ifTrue: [fileName]
		ifFalse: [fileName , extension].! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'TPR 5/10/1998 21:31'!
imageSuffix
"if 'image' is not suitable, override this message to return something that is ok"
	^'image'! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
localNameFor: fullName 
	"Return the local part the given name."
	self activeDirectoryClass
		splitName: fullName
		to: [:dirPath :localName | ^ localName]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 2/26/2010 10:56'!
sourceSuffix
"if 'sources' is not suitable, override this message to return something that is ok"
	^'sources'! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'bf 3/22/2000 18:04'!
splitName: fullName to: pathAndNameBlock
	"Take the file name and convert it to the path name of a directory and a local file name within that directory. FileName must be of the form: <dirPath><delimiter><localName>, where <dirPath><delimiter> is optional. The <dirPath> part may contain delimiters."

	| delimiter i dirName localName |
	delimiter _ self pathNameDelimiter.
	(i _ fullName findLast: [:c | c = delimiter]) = 0
		ifTrue:
			[dirName _ String new.
			localName _ fullName]
		ifFalse:
			[dirName _ fullName copyFrom: 1 to: (i - 1 max: 1).
			localName _ fullName copyFrom: i + 1 to: fullName size].

	^ pathAndNameBlock value: dirName value: localName! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:41'!
startUp

	Smalltalk openSourceFiles! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:09'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ self on: (Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]) ].
	^ CurrentDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 13:44'!
forFullFileName: fullFileName

	| path |
	path _ self dirPathFor: fullFileName.
	^ self on: path
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 4/19/2015 08:37'!
on: pathString
	"Return a new file directory for the given path, of the appropriate FileDirectory subclass for the current OS platform."

	| pathName |
	"If path ends with a delimiter (: or /) then remove it"
	((pathName _ pathString) endsWith: self activeDirectoryClass pathNameDelimiter asString) ifTrue: [
		pathName _ pathName copyFrom: 1 to: pathName size - 1].
	^ self activeDirectoryClass new setPathName: pathName
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jm 12/4/97 23:29'!
root
	"Answer the root directory."

	^ self on: ''
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:17'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)"

	ImageDirectory ifNil: [
		ImageDirectory _ self on: Smalltalk imagePath ].
	^ ImageDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:06'!
vmDirectory
	"Answer the directory containing the VM that runs us."

	VMDirectory ifNil: [
		VMDirectory _ self on: Smalltalk vmPath ].
	^ VMDirectory
! !

!FileDirectory class methodsFor: '*squeakCompatibility' stamp: 'jmv 6/17/2015 17:24'!
default
	"Old squeak protocol. Removed from Cuis to avoid false polymorphism and ambiguity
	(i.e. to make it easier to spot & change stuff)
	For Cuis code, use:
		#smalltalkImageDirectory (usually preferred for Smalltalk related files) or 
		#currentDirectory (usually preferred for other files, and especially for arguments of shell scripts)
	Answer #currentDirectory, even if Squeak answers #smalltalkImageDirectory, for consistency with
		FileStream fileNamed: 'filename.extension'
	without path, that is assumed to mean #currentDirectory."
	^ self currentDirectory! !

!FileDirectory class methodsFor: '*squeakCompatibility' stamp: 'jmv 6/17/2015 13:45'!
deleteFilePath: fileName
	"Delete the file after finding its directory"
	"este quiere un fullPath. Y el unico sender le manda un nombre simple...
	Modificarlo para que ande en ambos casos!! (incluso en freakin linux)
	directoryEntryFor: filenameOrPath
	"

	| dir fullName |
	
	fullName _ FileDirectory default fullNameFor: fileName.
	dir _ self forFullFileName: fullName.
	dir deleteFileNamed: (self localNameFor: fileName).
! !

!FileDirectory class methodsFor: '*squeakCompatibility' stamp: 'jmv 6/17/2015 13:14'!
directoryEntryFor: filenameOrPath
	^self default directoryEntryFor: filenameOrPath! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jm 3/27/98 08:17'!
dot
	"Return a one-character string containing the filename extension delimiter for this platform (i.e., the local equivalent of 'dot')"

	^ self extensionDelimiter asString
! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jm 3/27/98 06:57'!
extensionDelimiter
	"Return the character used to delimit filename extensions on this platform. Most platforms use the period (.) character."

	^ $.
! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 3/16/2010 19:23'!
isCaseSensitive
	"Return true if file names are treated case sensitive"
	"Warning: This method can't always be trusted, see comment in other implementors"
	^true! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'nk 3/13/2003 10:58'!
makeAbsolute: path
	"Ensure that path looks like an absolute path"
	^path first = self pathNameDelimiter
		ifTrue: [ path ]
		ifFalse: [ self slash, path ]! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 11/30/2014 16:23'!
makeRelative: path
	"Ensure that path looks like an relative path"
	^path first = self pathNameDelimiter
		ifTrue: [ path allButFirst ]
		ifFalse: [ path ]! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jm 5/8/1998 20:45'!
maxFileNameLength

	^ 31
! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 4/19/2015 08:38'!
pathNameDelimiter
	"return the active directory class's directory seperator character"
	"Warning: endless recursion if no specific #activeDirectoryClass found..."
	^ self activeDirectoryClass pathNameDelimiter! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'ar 4/18/1999 18:18'!
slash
	^ self pathNameDelimiter asString! !

!FileDirectory class methodsFor: 'create/delete file' stamp: 'jmv 6/17/2015 11:58'!
lookInUsualPlaces: fileName
	"Check the current directory, the imagePath, and the vmPath (and the vmPath's owner) for this file."

	| dir |
	((dir _ FileDirectory currentDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ FileDirectory smalltalkImageDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ FileDirectory vmDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ dir containingDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	^ nil! !

!FileDirectory class methodsFor: 'cacher state access' stamp: 'jmv 6/17/2015 11:05'!
releaseClassCachedState

	DirectoryClass _ nil.
	ImageDirectory _ nil.
	VMDirectory _ nil.
	CurrentDirectory _ nil! !

!FileDirectory class methodsFor: 'system start up' stamp: 'jmv 2/15/2008 00:45'!
shutDown

	Smalltalk closeSourceFiles.
! !

!AcornFileDirectory methodsFor: 'file name utilities' stamp: 'tpr 8/2/2003 19:34'!
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| fName badChars hasBadChars |
	fName _ super checkName: aFileName fixErrors: fixing.
	badChars _ #( $# $: $< $> $| $? $* $" $%) asSet.
	hasBadChars _ fName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue:[^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ fName].
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue:[$!!] 
				ifFalse:[char]]! !

!AcornFileDirectory methodsFor: 'file name utilities' stamp: 'ar 12/18/1999 00:47'!
fullPathFor: path
	path isEmpty ifTrue:[^pathName].
	((path includes: $$ ) or:[path includes: $:]) ifTrue:[^path].
	^pathName, self slash, path! !

!AcornFileDirectory methodsFor: 'testing' stamp: 'tpr 4/28/2004 21:54'!
directoryExists: filenameOrPath
"if the path is a root,we have to treat it carefully"
	(filenameOrPath endsWith: '$') ifTrue:[^(FileDirectory on: filenameOrPath) exists].
	^(self directoryNamed: filenameOrPath ) exists! !

!AcornFileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 13:19'!
entries
	"Return a collection of directory entries for the files and directories in 
	the directory with the given path. See primLookupEntryIn:index: for 
	further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries extraPath |
	entries _ super entries.
	pathName isEmpty
		ifTrue: [
			"For Acorn we also make sure that at least the parent of the current dir 
			is added - sometimes this is in a filing system that has not been (or 
			cannot be) polled for disc root names"
			extraPath _  self class smalltalkImageDirectory containingDirectory.
			"Only add the extra path if we haven't already got the root of the current dir in the list"
			(entries anySatisfy: [:ent | extraPath fullName beginsWith: ent name]) 
				ifFalse: [
					entries _ entries
								copyWith: (DirectoryEntryDirectory
										directory: self
										name: extraPath fullName
										creationTime: 0
										modificationTime: 0
										fileSize: 0) ]].
	^ entries! !

!AcornFileDirectory methodsFor: 'path access' stamp: 'tpr 11/30/2003 21:42'!
pathParts
	"Return the path from the root of the file system to this directory as an 
	array of directory names.
	This version tries to cope with the RISC OS' strange filename formatting; 
	filesystem::discname/$/path/to/file
	where the $ needs to be considered part of the filingsystem-discname atom."
	| pathList |
	pathList := super pathParts.
	(pathList indexOf: '$') = 2
		ifTrue: ["if the second atom is root ($) then stick $ on the first atom 
				and drop the second. Yuck"
			^ Array
				streamContents: [:a | 
					a nextPut: (pathList at: 1), '/$'.
					3 to: pathList size do: [:i | a
								nextPut: (pathList at: i)]]].
	^ pathList! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'jmv 2/15/2008 00:41'!
isActiveDirectoryClass
	"Does this class claim to be that properly active subclass of FileDirectory  
	for the current platform? On Acorn, the test is whether platformName 
	is 'RiscOS' (on newer VMs) or if the primPathNameDelimiter is $. (on
	older ones), which is what we would like to use for a dirsep if only it
	would work out. See pathNameDelimiter for more woeful details - then
	just get on and enjoy Squeak"

	^ Smalltalk platformName = 'RiscOS'
		or: [self primPathNameDelimiter = $.]! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'tpr 8/1/2003 16:38'!
isCaseSensitive
	"Risc OS ignores the case of file names"
	^ false! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'TPR 7/20/1999 17:52'!
maxFileNameLength

	^ 255
! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'TPR 5/10/1998 21:45'!
pathNameDelimiter
"Acorn RiscOS uses a dot as the directory separator and has no real concept of filename extensions. We tried to make code handle this, but there are just too many uses of dot as a filename extension - so fake it out by pretending to use a slash. The file prims do conversions instead.
Sad, but pragmatic"
	^ $/
! !

!DosFileDirectory methodsFor: 'as yet unclassified' stamp: 'di 6/18/1998 08:57'!
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| fName badChars hasBadChars |
	fName _ super checkName: aFileName fixErrors: fixing.
	badChars _ #( $: $< $> $| $/ $\ $? $* $") asSet.
	hasBadChars _ fName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue:[^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ fName].
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue:[$#] 
				ifFalse:[char]]! !

!DosFileDirectory methodsFor: 'as yet unclassified' stamp: 'bf 3/21/2000 17:06'!
setPathName: pathString
	"Ensure pathString is absolute - relative directories aren't supported on all platforms."

	(pathString isEmpty
		or: [pathString first = $\
			or: [pathString size >= 2 and: [pathString second = $: and: [pathString first isLetter]]]])
				ifTrue: [^ super setPathName: pathString].

	self error: 'Fully qualified path expected'! !

!DosFileDirectory methodsFor: 'path access' stamp: 'je 11/8/2000 20:02'!
driveName

   "return a possible drive letter and colon at the start of a Path name, empty string otherwise"

   | firstTwoChars |

   ( pathName size >= 2 ) ifTrue: [
      firstTwoChars _ (pathName copyFrom: 1 to: 2).
      (self class isDrive: firstTwoChars) ifTrue: [^firstTwoChars]
   ].
   ^''! !

!DosFileDirectory methodsFor: 'path access' stamp: 'nk 7/18/2004 17:26'!
fullNameFor: fileName
	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."
	fileName ifNil:[^fileName].
	"Check for fully qualified names"
	((fileName size >= 2 and: [fileName first isLetter and: [fileName second = $:]])
		or: [(fileName beginsWith: '\\') and: [(fileName occurrencesOf: $\) >= 2]])
			ifTrue:[^fileName].
	^super fullNameFor: fileName! !

!DosFileDirectory methodsFor: 'path access' stamp: 'je 11/8/2000 19:57'!
fullPathFor: path
	"Return the fully-qualified path name for the given file."
	path isEmpty ifTrue:[^pathName].
	(path at: 1) = $\ ifTrue:[
		(path size >= 2 and:[(path at: 2) = $\]) ifTrue:[^path]. "e.g., \\pipe\"
		^self driveName , path "e.g., \windows\"].
	(path size >= 2 and:[(path at: 2) = $: and:[path first isLetter]])
		ifTrue:[^path]. "e.g., c:"
	^pathName, self slash, path! !

!DosFileDirectory methodsFor: 'path access' stamp: 'nk 12/13/2002 10:05'!
relativeNameFor: path
	"Return the full name for path, assuming that path is a name relative to me."
	path isEmpty ifTrue:[^pathName].
	(path at: 1) = $\ ifTrue:[
		(path size >= 2 and:[(path at: 2) = $\]) ifTrue:[^super relativeNameFor: path allButFirst ]. "e.g., \\pipe\"
		^super relativeNameFor: path "e.g., \windows\"].
	(path size >= 2 and:[(path at: 2) = $: and:[path first isLetter]])
		ifTrue:[^super relativeNameFor: (path copyFrom: 3 to: path size) ]. "e.g., c:"
	^pathName, self slash, path! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'ar 5/1/1999 01:48'!
isCaseSensitive
	"Return true if file names are treated case sensitive"
	^false! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'ar 3/6/2004 03:46'!
isDrive: fullName
	"Answer whether the given full name describes a 'drive', e.g., one of the root directories of a Win32 file system. We allow two forms here - the classic one where a drive is specified by a letter followed by a colon, e.g., 'C:', 'D:' etc. and the network share form starting with double-backslashes e.g., '\\server'."
	^ (fullName size = 2 and: [fullName first isLetter and: [fullName last = $:]])
		or: [(fullName beginsWith: '\\') and: [(fullName occurrencesOf: $\) = 2]]! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'jm 5/8/1998 20:45'!
maxFileNameLength

	^ 255
! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'jm 12/4/97 22:57'!
pathNameDelimiter

	^ $\
! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'ar 3/6/2004 04:14'!
splitName: fullName to: pathAndNameBlock
	"Take the file name and convert it to the path name of a directory and a local file name within that directory. 
	IMPORTANT NOTE: For 'drives', e.g., roots of the file system on Windows we treat the full name of that 'drive' as the local name rather than the path. This is because conceptually, all of these 'drives' hang off the virtual root of the entire Squeak file system, specified by FileDirectory root. In order to be consistent with, e.g., 

		DosFileDirectory localNameFor: 'C:\Windows' -> 'Windows'
		DosFileDirectory dirPathFor: 'C:\Windows' -> 'C:'

	we expect the following to be true:

		DosFileDirectory localNameFor: 'C:' -> 'C:'
		DosFileDirectory dirPathFor: 'C:'. -> ''
		DosFileDirectory localNameFor: '\\server' -> '\\server'.
		DosFileDirectory dirPathFor: '\\server' -> ''.

	so that in turn the following relations hold:

		| fd |
		fd := DosFileDirectory on: 'C:\Windows'.
		fd containingDirectory includes: fd localName.
		fd := DosFileDirectory on: 'C:'.
		fd containingDirectory includes: fd localName.
		fd := DosFileDirectory on: '\\server'.
		fd containingDirectory includes: fd localName.
	"
	(self isDrive: fullName)
		ifTrue: [^ pathAndNameBlock value:''  value: fullName].
	^ super splitName: fullName to: pathAndNameBlock! !

!UnixFileDirectory methodsFor: 'testing' stamp: 'sr 5/8/2000 12:58'!
directoryExists: filenameOrPath
	"Handles the special case of testing for the root dir: there isn't a
	possibility to express the root dir as full pathname like '/foo'."

	^ filenameOrPath = '/' or: [super directoryExists: filenameOrPath]! !

!UnixFileDirectory methodsFor: 'testing' stamp: 'sr 5/8/2000 13:03'!
fileOrDirectoryExists: filenameOrPath 
	"Handles the special case of testing for the root dir: there isn't a 
	possibility to express the root dir as full pathname like '/foo'."

	^ filenameOrPath = '/' or: [super fileOrDirectoryExists: filenameOrPath]! !

!UnixFileDirectory methodsFor: 'file names' stamp: 'bf 3/22/2000 18:24'!
fullPathFor: path
	"Return the fully-qualified path name for the given file."
	path isEmpty ifTrue: [^ pathName].
	path first = $/ ifTrue: [^ path].
	^ pathName = '/'			"Only root dir ends with a slash"
		ifTrue: ['/' , path]
		ifFalse: [pathName , '/' , path]! !

!UnixFileDirectory methodsFor: 'private' stamp: 'jm 12/4/97 23:43'!
setPathName: pathString
	"Unix path names start with a leading delimiter character."

	(pathString isEmpty or: [pathString first ~= self pathNameDelimiter])
		ifTrue: [pathName _ self pathNameDelimiter asString, pathString]
		ifFalse: [pathName _ pathString].
! !

!UnixFileDirectory class methodsFor: 'platform specific' stamp: 'jmv 3/16/2010 19:24'!
isCaseSensitive
	"Warning: This method can't always be trusted.
	For instance, the Mac supports both case sensitive and case insensitive volumes.
	Something better needs to be done."
	^ Smalltalk platformName ~= 'Mac OS'! !

!UnixFileDirectory class methodsFor: 'platform specific' stamp: 'yo 2/4/1999 06:40'!
maxFileNameLength

	^ 255! !

!UnixFileDirectory class methodsFor: 'platform specific' stamp: 'jm 9/17/97 15:48'!
pathNameDelimiter

	^ $/
! !

!SmalltalkImage class methodsFor: 'as yet unclassified' stamp: 'jmv 5/8/2015 09:46'!
current
	^Smalltalk! !

!SqDirectoryEntry methodsFor: 'testing' stamp: 'jmv 5/27/2014 11:22'!
= aDirectoryEntry 

	"Any object is equal to itself"
	self == aDirectoryEntry ifTrue: [ ^ true ].

	"Answer whether I am equivalent in all of my file-system attributes."
	self species == aDirectoryEntry species ifFalse: [ ^ false ].

	^ self containingDirectory = aDirectoryEntry containingDirectory
		and: [ self name = aDirectoryEntry name
				and: [ self modificationTime = aDirectoryEntry modificationTime
						and: [ self fileSize = aDirectoryEntry fileSize ]]]! !

!SqDirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/21/2011 21:50'!
exists
	^ (self containingDirectory
		entryAt: self name
		ifAbsent: [ nil ]) notNil! !

!SqDirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/18/2011 16:04'!
hash
	"Hashing on directory + name should be sufficient."
	^ (self containingDirectory hash hashMultiply + self name hash) hashMultiply! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 10/30/2009 15:50'!
baseName
	^ FileDirectory baseNameFor: self name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 9/12/2007 17:36'!
containingDirectory
	"Answer the FileDirectory in which I reside."
	^ directory! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 8/10/2007 12:25'!
creationDateAndTime
	"The DateAndTime my entry in the file system was created."
	^DateAndTime fromSeconds: creationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 2/15/2010 13:16'!
creationTime
	"The time the entry was created, as an Integer number of seconds offset from the DateAndTime epoch."
	^creationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 9/21/2009 18:24'!
extension
	^ FileDirectory extensionFor: self name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'ls 7/15/1998 21:38'!
fileSize
	"size of the entry, if it's a file"
	^fileSize! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'jmv 1/7/2014 00:46'!
fileSizeString
	"Answer my file size as an easy-to-read String."
	^ self fileSize printStringAsBytes! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'bgf 9/9/2010 07:36'!
fullName
	"The fully-qualified name.
	 Since this method falls into the equality test, make it safe when directory is nil."
	^ directory 
		ifNotNil: [ directory fullNameFor: self name ] 
		ifNil: [ self name ]! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 9/12/2007 17:25'!
isDirectory
	"whether this entry represents a directory"
	self subclassResponsibility! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 8/10/2007 12:25'!
modificationDateAndTime
	"The DateAndTime my entry in the file system was last modified."
	^ DateAndTime fromSeconds: modificationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'ls 7/15/1998 21:37'!
modificationTime
	"time the entry was last modified"
	^modificationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'ls 7/15/1998 21:37'!
name
	"name of the entry"
	^name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 8/29/2007 17:44'!
printOn: aStream 
	super printOn: aStream.
	aStream
		space ;
		nextPutAll: self name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 1/25/2011 13:16'!
size
	"For API compatibility with byte objects (for streaming api)."
	^ self fileSize! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'bp 6/2/2014 10:49'!
splitNameVersionExtension
	" answer an array with the root name, version # and extension."
	^directory splitNameVersionExtensionFor: self name! !

!SqDirectoryEntry methodsFor: 'private-initialization' stamp: 'cmm 9/12/2007 17:20'!
setDirectory: aFileDirectoryOrServerDirectory name: name0  creationTime: creationTime0  modificationTime: modificationTime0 fileSize: fileSize0
	directory := aFileDirectoryOrServerDirectory.
	name := name0.
	creationTime := creationTime0.
	modificationTime := modificationTime0.
	fileSize := fileSize0! !

!DirectoryEntryDirectory methodsFor: 'convert' stamp: 'jmv 6/6/2012 14:23'!
asFileDirectory
	"Answer a FileDirectory representing the same directory I represent."
	^directory on: (directory fullNameFor: self name)! !

!DirectoryEntryDirectory methodsFor: 'testing' stamp: 'cmm 9/13/2007 12:24'!
isDirectory
	"whether this entry represents a directory, it does."
	^ true! !

!DirectoryEntryFile methodsFor: 'testing' stamp: 'cmm 9/13/2007 12:24'!
isDirectory
	"whether this entry represents a directory, it does not."
	^ false! !

!DirectoryEntryFile methodsFor: 'stream access' stamp: 'jmv 6/6/2012 14:21'!
readStreamDo: aBlock
	"Obtain a FileStream on my contents that can be read, but not written,
	and send it to aBlock."
	^ directory readOnlyFileNamed: self name do: aBlock! !

!ProtoObject methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:02'!
ifNotNilDo: ifNotNilBlock
	"Compatibility. Prefer #ifNotNil:"
	^self ifNotNil: ifNotNilBlock! !

!Object methodsFor: '*squeakCompatibility' stamp: 'jmv 5/14/2015 11:11'!
isCharacter
	^false! !

!UndefinedObject methodsFor: '*squeakCompatibility' stamp: 'jmv 3/1/2010 09:59'!
environment
	"Necessary to support disjoint class hierarchies."

	^Smalltalk		"No environments in Cuis..."! !

!Behavior methodsFor: '*squeakCompatibility' stamp: 'jmv 11/1/2011 23:15'!
environment
	"Return the environment in which the receiver is visible"
	^Smalltalk! !

!Float methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:08:16'!
printOn: aStream base: base digitCount: numDigits

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base digitCount: numDigits]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base digitCount: numDigits]]! !

!Float methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:24:25'!
printOn: aStream base: base nDigits: numDigits

	self printOn: aStream base: base digitCount: numDigits! !

!Time class methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 10:52'!
totalSeconds
	^self primLocalSecondsClock ! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 9/6/2016 10:27:29'!
asInteger
	"Answer the value of the receiver."

	^self numericValue! !

!Character methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:26'!
asUnicode
	"Answer the unicode encoding of the receiver"
	"self leadingChar = 0 ifTrue: [^ self asInteger].
	^self encodedCharSet charsetClass convertToUnicode: self charCode"
	^self asInteger! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 11/27/2017 11:04:11'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self numericValue >= min numericValue and: [self numericValue <= max numericValue]! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 5/14/2015 11:12'!
isCharacter
	^true! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 9/6/2016 10:27:11'!
value
	^self numericValue! !

!Character class methodsFor: '*squeakCompatibility' stamp: 'jmv 9/6/2016 10:26:47'!
value: anInteger
	"Answer the Character whose value is anInteger."

	^ self numericValue: anInteger! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg
	"Activate the receiver, with one or zero arguments."
	
	numArgs >= 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg
	"Activate the receiver, with two or less arguments."
	
	numArgs >= 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg cull: thirdArg
	"Activate the receiver, with three or less arguments."
	
	numArgs >= 2 ifTrue: [ 
		numArgs >= 3 ifTrue: [ ^self value: firstArg value: secondArg value: thirdArg ].
		^self value: firstArg value: secondArg ].
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg
	"Activate the receiver, with four or less arguments."
	
	numArgs >= 3 ifTrue: [
		numArgs >= 4 ifTrue: [
			^self value: firstArg value: secondArg value: thirdArg value: fourthArg ].
		^self value: firstArg value: secondArg value: thirdArg ].
	numArgs = 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!InputSensor methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 12:47'!
cursorPoint
	^self mousePoint! !

!SequenceableCollection methodsFor: '*squeakCompatibility' stamp: 'jmv 4/2/2016 23:18'!
doWithIndex: elementAndIndexBlock
	"Old style. Prefer #withIndexDo:"
	self withIndexDo: elementAndIndexBlock! !

!String methodsFor: '*squeakCompatibility' stamp: 'eem 2/3/2015 12:04'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| char result sourceStream subString |
	#Collectn.
	"Changed 2000/04/08 For ANSI <readableString> protocol."
	(separators isString or:[separators allSatisfy: [:element | element isCharacter]]) ifFalse:
		[^ self error: 'separators must be Characters.'].
	sourceStream := ReadStream on: self.
	result := OrderedCollection new.
	subString := String new.
	[sourceStream atEnd]
		whileFalse: 
			[char := sourceStream next.
			(separators includes: char)
				ifTrue: [subString notEmpty
						ifTrue: 
							[result add: subString copy.
							subString := String new]]
				ifFalse: [subString := subString , (String with: char)]].
	subString notEmpty ifTrue: [result add: subString copy].
	^ result asArray! !

!String methodsFor: '*squeakCompatibility' stamp: 'hjh 5/9/2015 17:03'!
withCRs
	"Return a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c = $\ ifTrue: [ Character cr ] ifFalse: [ c ]].! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
cr
	^self crString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
crlf
	^self crlfString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
lf
	^self lfString! !

!Symbol methodsFor: '*squeakCompatibility' stamp: 'hjh 5/9/2015 17:37'!
value: anObject 
	^anObject perform: self.! !

!CompiledMethod methodsFor: '*squeakCompatibility' stamp: 'NS 12/12/2003 15:18'!
isAbstract
	| marker |
	marker := self markerOrNil.
	^ marker notNil and: [self class abstractMarkers includes: marker].! !

!CompiledMethod methodsFor: '*squeakCompatibility' stamp: 'al 2/13/2006 17:44'!
markerOrNil
	"If I am a marker method, answer the symbol used to mark me.  Otherwise
	answer nil.

	What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	used to indicate ('mark') a special property.

	Marker methods compile to bytecode like:

		9 <70> self
		10 <D0> send: <literal 1>
		11 <87> pop
		12 <78> returnSelf

	for the first form, or 

		9 <70> self
		10 <D0> send: <literal 1>
		11 <7C> returnTop

	for the second form."

	| e |
	((e := self endPC) = 19 or: [e = 20]) ifFalse: [^ nil].
	(self numLiterals = 3) ifFalse:[^ nil].
	(self at: 17) =  16r70 ifFalse:[^ nil].		"push self"
	(self at: 18) = 16rD0 ifFalse:[^ nil].		"send <literal 1>"
	"If we reach this point, we have a marker method that sends self <literal 1>"
	^ self literalAt: 1
! !

!CompiledMethod class methodsFor: '*squeakCompatibility' stamp: 'NS 12/12/2003 15:17'!
abstractMarkers
	^ #(subclassResponsibility shouldNotImplement)! !

!Stream methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:02:52'!
print: aNumber digits: numDigits

	aNumber printOn: self base: 10 nDigits: numDigits ! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 14:59:03'!
cr
	"Append a lf character to the receiver.
	Use this method when you specifically need a cr character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character cr! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 14:58'!
crtab
	"Append a return character, followed by anInteger tab characters, to the 
	receiver."

	self cr; tab! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 14:59'!
crtab: anInteger
	"Append a return character, followed by anInteger tab characters, to the 
	receiver."

	self cr.
	anInteger timesRepeat: [self tab]! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'jmv 12/23/2013 09:34'!
lf
	"Append a lf character to the receiver.
	Use this method when you specifically need a lf character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character lf! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 14:59:34'!
nl
	self newLine ! !

!StandardFileStream methodsFor: '*squeakCompatibility' stamp: 'jmv 4/2/2016 23:38'!
lineEndConvention: aSymbol
	"ignore it"! !

!StandardFileStream class methodsFor: '*squeakCompatibility' stamp: 'jmv 5/31/2016 12:38'!
forceNewFileNamed: aFilename
	^ aFilename asFileEntry forceWriteStream! !

!Encoder methodsFor: '*squeakCompatibility' stamp: 'jmv 3/1/2010 11:18'!
environment
	"Answer the environment of the current compilation context,
	 be it in a class or global (e.g. a workspace)"
	^Smalltalk "No environments in Cuis..."! !

!Transcript class methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 12:41'!
cr
	self newLine! !

!Transcript class methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:04:14'!
nl
	self newLine! !

!Transcript class methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:04:19'!
print: aNumber digits: numDigits

	aNumber printOn: self base: 10 nDigits: numDigits ! !
KlattResonatorIndices initialize!
