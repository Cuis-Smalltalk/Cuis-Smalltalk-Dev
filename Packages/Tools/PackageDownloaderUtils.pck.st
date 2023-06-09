'From Cuis 6.0 [latest update: #5832] on 9 June 2023 at 12:19:41 am'!
'Description Tool for processing the list of packages that PackageDownloader uses.'!
!provides: 'PackageDownloaderUtils' 1 11!
!requires: 'PackageDownloader' 1 25 nil!
SystemOrganization addCategory: 'PackageDownloaderUtils'!


!classDefinition: #PackageDownloaderUtils category: 'PackageDownloaderUtils'!
Object subclass: #PackageDownloaderUtils
	instanceVariableNames: 'packageList csvFileUrl csvFile jsonFile skipErrors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PackageDownloaderUtils'!
!classDefinition: 'PackageDownloaderUtils class' category: 'PackageDownloaderUtils'!
PackageDownloaderUtils class
	instanceVariableNames: ''!


!PackageDownloaderUtils commentStamp: 'MM 6/9/2023 00:19:15' prior: 0!
I download a file with a list of packages in CSV format, and produce a JSON file with information of the packages, that can be used with PackageDownloader.

The procedure works as follows:

- A CSV file is downloaded from the specified url.
- The packages listed in the CSV files are downloaded to some directory.
- The directory with the packages is scanned to fetch package information.
- A JSON file is produced.

That JSON file should be uploaded to an url address where PackageDownloader downloads the package list.

If an error ocurrs when downloading or scanning a package, it is skipped and discarded from the final package list by default.

Example usage:

PackageDownloaderUtils new
	csvFileUrl: 'https://gist.githubusercontent.com/mmontone/f43e8c36fa8d3954163289b40670b1d9/raw/cuis-packages.csv?c=', 1000 atRandom printString;
	csvFile: '/home/marian/src/Cuis/packages.csv';
	jsonFile: '/home/marian/src/Cuis/packages.json';
	run.!

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/19/2021 12:38:30'!
addPackage: aPackageSpec

	packageList add: aPackageSpec! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/21/2021 20:49:56'!
clearPackageList
	packageList _ OrderedCollection new! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/20/2021 20:38:10'!
collectPackagesFromDirectory: aDirectory

	"Create a collection of PackageSpec with package files found in aDirectory."
	
	|packageFiles|
	
	packageFiles _ aDirectory fileNamesMatching: '*.pck.st'.
		
	packageFiles do: [:packageFile | | packageSpec codePackage |
		codePackage _ CodePackageFile onFileEntry: (aDirectory // packageFile).
		packageSpec _ self specFromCodePackage: codePackage.
		self addPackage: packageSpec]
			
	! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/25/2021 21:20:39'!
collectPackagesFromJsonFile: aJsonFile

	aJsonFile readStreamDo: [:fs | |ps|
		ps _ Json readFrom: fs.
		
		ps do: [:p | |package|
			package _ RemotePackageDescription new.
			package name: (p at: 'name');
					description: (p at: 'description');
					downloadUrl: (p at: 'downloadUrl');
					homepage: (p at: 'homepage').
			self addPackage: package]]
					
			! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/25/2021 21:25:30'!
collectPackagesFromUrls: urls

	| dir |
	dir _ '/home/marian/src/Cuis/PackageInstaller/packages' asDirectoryEntry.
	urls do: [ :url | | filePath |
		filePath _ dir // (url at: 'fileName').
		filePath fileContents: (WebClient httpGet: (url at: 'url')) content ].! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 6/9/2023 00:18:14'!
collectRemotePackagesFromFile: aCSVFile

	aCSVFile readStreamDo: [ :f | | line splitLine packageName downloadUrl packageFile |
		
		[f atEnd] whileFalse: [ | package codePackage |
			[line := f nextLine.
			splitLine := line findTokens: (Array with: $,).
			packageName := splitLine first.
			downloadUrl := splitLine second.
			packageFile := PackageDownloaderUtils downloadDirectory // (packageName, '.pck.st').
			
			self downloadPackage: downloadUrl into: packageFile.
			
			codePackage := CodePackageFile onFileEntry: packageFile.
			
			package := RemotePackageDescription new.
			package downloadUrl: downloadUrl.
			self populatePackage: package from: codePackage.
			
			self addPackage: package]
				on: Error do: [:e |
					skipErrors 
						ifTrue: [	Transcript show: ('Error processing package: ', packageName, '. Skipping.'); newLine.
								Transcript show: e description; newLine.]
						ifFalse: [self error: 'Error processing package: ', packageName, ' ' , e messageText]	]]]! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 12/17/2021 21:46:49'!
defaultCSVFile

	^ ((CodePackage named: 'PackageDownloaderUtils' createIfAbsent: false registerIfNew: false)
		packageDirectory // 'packages.csv') asString! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 12/17/2021 21:46:57'!
defaultJSONFile

	^ ((CodePackage named: 'PackageDownloaderUtils' createIfAbsent: false registerIfNew: false)
		packageDirectory // 'packages.json') asString! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/28/2021 14:42:18'!
downloadPackage: url into: aFileEntry

	| response |
	
	Transcript show: 'Downloading ', url, ' ... '; newLine.
	
	response _ WebClient httpGet: url.
	(response code >=400) ifTrue: [self error: ('Error downloading: {1}. HTTP status: {2}'
										format: {url. response code})].
									
	aFileEntry fileContents: response content! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/20/2021 20:34:26'!
downloadPackagesFromUrls: arg1
	| temp2 |
	temp2 _ '/home/marian/src/Cuis/PackageInstaller/packages' asDirectoryEntry.
	arg1 do: [ :argm1_3 | | temp1_4 |
		Transcript
			show: 'Downloading ' , (argm1_3 at: 'name') , ' ... ';
			newLine.
		temp1_4 _ temp2 // ((argm1_3 at: 'name'), '.pck.st').
		temp1_4 fileContents: (WebClient httpGet: (argm1_3 at: 'url')) content ].! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/20/2021 20:49:31'!
downloadPackagesFromUrls: arg1 into: aDirectory
	
	arg1 do: [ :argm1_3 | | temp1_4 |
		Transcript
			show: 'Downloading ' , (argm1_3 at: 'name') , ' ... ';
			newLine.
		temp1_4 _ aDirectory // ((argm1_3 at: 'name'), '.pck.st').
		temp1_4 fileContents: (WebClient httpGet: (argm1_3 at: 'url')) content ].! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 6/9/2023 00:17:47'!
initialize
	packageList := OrderedCollection new.
	skipErrors := true.! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/19/2021 12:06:44'!
package: packageName properties: aBlock

	|package|
	
	package _ PackageSpec new
				name: packageName;
				yourself.
	aBlock value: package.
	self addPackage: package! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/19/2021 12:04:43'!
packageList
	^ packageList! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/26/2021 09:39:09'!
populatePackage: aPackageSpec from: aCodePackage

	"Populate a PackageSpec from aCodePackage."
	
	aPackageSpec packageName: aCodePackage packageName;
		description: aCodePackage packageDescription;
		requires: aCodePackage requires;
		provides: aCodePackage provides.
	
	^ aPackageSpec! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 6/8/2023 18:47:13'!
run

	"Download list of packages"
 
	Transcript show: 'Downloading list of packages from: '; show: csvFileUrl; newLine.
	self csvFile asFileEntry fileContents: (WebClient httpGet: csvFileUrl) content.
	
	"Process the list of packages to generate json definitions"
	Transcript show: 'Downloading and scanning packages...'; newLine.
	self	collectRemotePackagesFromFile: self csvFile asFileEntry.
	
	Transcript show: 'Writing list of packages to JSON file: '; show: self jsonFile; newLine.
	self writePackagesListToJsonFile: self jsonFile asFileEntry.! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 6/8/2023 20:58:22'!
skipErrors: aBoolean

	skipErrors := aBoolean! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/19/2021 14:33:49'!
specFromCodePackage: aCodePackage

	"Create a PackageSpec from aCodePackage."
	
	| packageSpec |
	
	packageSpec _ FilePackageSpec new.
	
	packageSpec name: aCodePackage packageName;
		description: (String streamContents: [:s | s nextPutAll: aCodePackage packageDescription;
											newLine;
											newLine;
											nextPutAll: aCodePackage description]);
		requires: aCodePackage requires;
		provides: aCodePackage provides;
		codePackageFile: aCodePackage.
	
	^ packageSpec! !

!PackageDownloaderUtils methodsFor: 'as yet unclassified' stamp: 'MM 11/25/2021 22:20:55'!
writePackagesListToJsonFile: aFileEntry

	aFileEntry writeStreamDo: [:file |
		self packageList jsonWriteOn: file].! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:45:29'!
csvFile
	"Answer the value of csvFile"

	^ csvFile ifNil: [self defaultCSVFile]! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:38:38'!
csvFile: anObject
	"Set the value of csvFile"

	csvFile _ anObject! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:38:38'!
csvFileUrl
	"Answer the value of csvFileUrl"

	^ csvFileUrl! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:38:38'!
csvFileUrl: anObject
	"Set the value of csvFileUrl"

	csvFileUrl _ anObject! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:45:44'!
jsonFile
	"Answer the value of jsonFile"

	^ jsonFile ifNil: [self defaultJSONFile]! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:38:38'!
jsonFile: anObject
	"Set the value of jsonFile"

	jsonFile _ anObject! !

!PackageDownloaderUtils methodsFor: 'accessing' stamp: 'MM 12/17/2021 21:38:38'!
packageList: anObject
	"Set the value of packageList"

	packageList _ anObject! !

!PackageDownloaderUtils class methodsFor: 'as yet unclassified' stamp: 'MM 12/17/2021 21:43:19'!
downloadDirectory

	^ ((CodePackage named: 'PackageDownloaderUtils' createIfAbsent: false registerIfNew: false)
		packageDirectory / 'packageDownloaderPackages')
			assureExistence ;
			yourself! !

!WebClient methodsFor: '*PackageDownloaderUtils' stamp: 'MM 11/26/2021 16:43:49'!
httpPatch: urlString content: postData type: contentType do: aBlock
	"PUT the data to the given url"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'PATCH'.
	contentType ifNotNil:[request headerAt: 'Content-Type' put: contentType].
	request headerAt: 'Content-Length' put: postData size.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request content: postData readStream size: postData size! !
