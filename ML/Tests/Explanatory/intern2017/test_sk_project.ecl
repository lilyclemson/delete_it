
// Presents K-Means clustering in a 2-dimensional space. 100 data points
// are initialized with random values on the x and y axes, and 4 centroids
// are initialized with values assigned to be regular but non-symmetrical.
//
// The sample code shows how to determine the new coordinates of the
// centroids after a user-defined set of iterations. Also shows how to
// determine the "allegiance" of each data point after those iterations.
//---------------------------------------------------------------------------

//K-Means testing file: summer intern 2017

IMPORT ML;
IMPORT excercise.irisset as irisset;
IMPORT excercise.kegg;
IMPORT excercise.uscensus as uscensus;
IMPORT excercise.dp100 as dp100;
IMPORT excercise.gassensor as gas;


//dp100
// dDocumentMatrix := dp100.Documents;
// dCentroidMatrix := dp100.Centroids;


//iris
// dDocumentMatrix := irisset.input;
// dCentroidMatrix := irisset.input[1..4];

//KEGG

dDocumentMatrix := kegg.input;
// dCentroidMatrix := kegg.input[1..100];

dDoc_len := COUNT(dDocumentMatrix);

INTEGER ranger(INTEGER lower, INTEGER upper) := FUNCTION
RETURN lower + random()% (upper - lower);
END;

d := record
INTEGER id;
END;

kSet := DATASET('~lili::kset1077936200', d, thor);
output(kSet);
dCentroidMatrix := dDocumentMatrix(dDocumentMatrix.id IN SET(kSet,id));
 
//uscensus
// dDocumentMatrix := uscensus.input;
// dCentroidMatrix := uscensus.input[1..301];

// dDocumentMatrix := gas.dGassensor;
// dCentroidMatrix := gas.dGassensor[1..4];
 

ML.ToField(dDocumentMatrix,dDocuments);
ML.ToField(dCentroidMatrix,dCentroids);


KMeans:=ML.Cluster.KMeans(dDocuments,dCentroids,40,0.3);

OUTPUT(KMeans.Allresults, NAMED('KMeansAllresults'));                                      
OUTPUT(KMeans.Convergence, NAMED('KMeansTotal_Iterations')); 

//OUTPUT(KMeans.Allegiances(), NAMED('KMeansAllegiances'));



