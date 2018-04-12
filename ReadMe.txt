

	Firstly go into the file PackageInstaller and run all of them.

	Then the running order is this:

	MLManager -> DataManager -> DataReader
			                        -> DataProcessing
		   	                        -> DataNormalization (for both scalling and normalisation)
				            -> DataSplitter
			   
	                    -> FeatureSelection -> FeatureSelectionFilter
				                 -> FeatureSelectionWrapper
			          		     -> FeatureSelectionEmbedded

	Then chose an algorithm from ML Manager to run (let's say SVM):
			  -> SupportVectorMachines -> MLSVM


	For data and results interpretation:
	 1. Use ResultsBinder to create a new dataset called all.results
	 2. Run what plot you need from ResultsInterpreter}