'From Cuis 5.0 of 7 November 2016 [latest update: #3267] on 11 March 2018 at 9:05:37 am'!

!Feature methodsFor: 'testing' stamp: 'jmv 3/11/2018 09:03:09'!
satisfies: featureRequirement
	"Does this provided Feature satisfy the FeatureRequirement?"
	
	"Must match name."
	^ (name sameAs: featureRequirement name) and: [

		"If no specific version req, we are done. Ok."
		featureRequirement minVersion isNil or: [

		"If our version is exactly the min req version, we must also satisfy minRevision"
		version = featureRequirement minVersion and: [
			featureRequirement minRevision isNil or: [ revision >= featureRequirement minRevision ]]] or: [
		
		"If we are past min req version, ignore minRevision, but check we are not beyond max req version"
		version > featureRequirement minVersion and: [
			featureRequirement maxVersion isNil or: [ version <= featureRequirement maxVersion ]]]]! !

