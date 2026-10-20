'From Cuis7.9 [latest update: #0] on 20 October 2026 at 1:33:11 pm'!

!Feature class methodsFor: 'convenience' stamp: 'CAP 20/Oct/2026, 4:32:50 pm (UTC)'!
require: featureName versionNumber: versionNumber
	"
	Feature require: 'Sound' versionNumber: 1
	"
	(FeatureRequirement name: featureName version: (SemanticVersion major: versionNumber)) require! !

!methodRemoval: Feature class #require:version: stamp: 'CAP 20/Oct/2026, 4:32:59 pm (UTC)'!
Feature class removeSelector: #require:version:!
