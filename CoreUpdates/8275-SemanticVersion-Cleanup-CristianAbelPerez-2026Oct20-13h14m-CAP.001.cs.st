'From Cuis7.9 [latest update: #0] on 20 October 2026 at 1:27:48 pm'!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision versionNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion maxVersion minRevision pathName codePackageFile minVersionNumber maxVersionNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'version versionMajor versionMinor releaseDate updatesPrefix highestUpdate lastUpdateDate '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-Support'!
!methodRemoval: SystemVersion #versionRevision stamp: 'CAP 20/Oct/2026, 4:14:57 pm (UTC)'!
SystemVersion removeSelector: #versionRevision!
!methodRemoval: FeatureRequirement class #name:version:revision: stamp: 'CAP 20/Oct/2026, 4:15:49 pm (UTC)'!
FeatureRequirement class removeSelector: #name:version:revision:!
!methodRemoval: FeatureRequirement class #name:minVersion:minRevision:maxVersion: stamp: 'CAP 20/Oct/2026, 4:16:02 pm (UTC)'!
FeatureRequirement class removeSelector: #name:minVersion:minRevision:maxVersion:!
!methodRemoval: FeatureRequirement #minRevision stamp: 'CAP 20/Oct/2026, 4:15:03 pm (UTC)'!
FeatureRequirement removeSelector: #minRevision!
!methodRemoval: FeatureRequirement #minVersionNumber stamp: 'CAP 20/Oct/2026, 4:17:18 pm (UTC)'!
FeatureRequirement removeSelector: #minVersionNumber!
!methodRemoval: FeatureRequirement #maxVersionNumber stamp: 'CAP 20/Oct/2026, 4:17:23 pm (UTC)'!
FeatureRequirement removeSelector: #maxVersionNumber!
!methodRemoval: FeatureRequirement #name:minVersion:minRevision:maxVersion: stamp: 'CAP 20/Oct/2026, 4:16:05 pm (UTC)'!
FeatureRequirement removeSelector: #name:minVersion:minRevision:maxVersion:!
!methodRemoval: Feature class #require:version:revision: stamp: 'CAP 20/Oct/2026, 4:15:10 pm (UTC)'!
Feature class removeSelector: #require:version:revision:!
!methodRemoval: Feature class #name:version:revision: stamp: 'CAP 20/Oct/2026, 4:15:39 pm (UTC)'!
Feature class removeSelector: #name:version:revision:!
!methodRemoval: Feature #revision stamp: 'CAP 20/Oct/2026, 4:15:12 pm (UTC)'!
Feature removeSelector: #revision!
!methodRemoval: Feature #newVersion stamp: 'CAP 20/Oct/2026, 4:15:15 pm (UTC)'!
Feature removeSelector: #newVersion!
!methodRemoval: Feature #name:version:revision: stamp: 'CAP 20/Oct/2026, 4:15:44 pm (UTC)'!
Feature removeSelector: #name:version:revision:!
!methodRemoval: Feature #versionNumber stamp: 'CAP 20/Oct/2026, 4:16:38 pm (UTC)'!
Feature removeSelector: #versionNumber!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion maxVersion pathName codePackageFile'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'version releaseDate updatesPrefix lastUpdateDate'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!
