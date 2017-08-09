IMPORT ML;
IMPORT ML.Types as Types;
IMPORT ML.Mat as Mat;
IMPORT excercise.uscensus as uscensus;
IMPORT excercise.kegg;
//KEGG
dDocumentMatrix := kegg.input;

// uscensus
// dDocumentMatrix := uscensus.input;



dDoc_len := COUNT(dDocumentMatrix);
k := 30;
g := k/15;// t

INTEGER ranger(INTEGER lower, INTEGER upper) := FUNCTION
RETURN lower + random()% (upper - lower);
END;


d := record
INTEGER id;
END;

r := RANDOM();

kSet := DATASET(k, TRANSFORM(d, SELF.id := ranger(1, dDoc_len)));
output(kset, , '~lili::kset' + r);
// OUTPUT(kSet,d,'~kset');
dCentroidMatrix := dDocumentMatrix(dDocumentMatrix.id IN SET(kSet,id));


gSet := DATASET(g, TRANSFORM(d, SELF.id := RANDOM()%k));
// dCentroidMatrix( dCentroidMatrix.id IN SET(gSet,id));
dGt := PROJECT(gSet, transform(RECORDOF(dCentroidMatrix), self := dCentroidMatrix[left.id]));
output(gset, , '~lili::gset' + r);
output(dGt);
// OUTPUT(gSet,d,'~gset');


ML.ToField(dDocumentMatrix,dDocuments);
ML.ToField(dCentroidMatrix,dCentroids);
ML.ToField(dGt, dt)

ite :=100;
#WORKUNIT('name', 'yinyangtest'); 
YinyangKMeans:=ML.yinyang.drafts.yinyangkmeans_optimization1.YinyangKMeans(dDocuments,dCentroids,dt,ite,0.3);

OUTPUT(YinyangKMeans.Allresults, NAMED('YinyangAllresults'));
OUTPUT(YinyangKMeans.Convergence,NAMED('YinyangKMeansTotal_Iterations'));


KMeans:=ML.Cluster.KMeans(dDocuments,dCentroids,ite,0.3);

OUTPUT(KMeans.Allresults, NAMED('KMeansAllresults'));                                      
OUTPUT(KMeans.Convergence, NAMED('KMeansTotal_Iterations'));
