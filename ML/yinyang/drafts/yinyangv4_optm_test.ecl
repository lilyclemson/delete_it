IMPORT ML;
IMPORT ML.Types as Types;
IMPORT ML.Mat as Mat;
IMPORT excercise.irisset as irisset;
IMPORT excercise.kegg;
IMPORT excercise.uscensus as uscensus;
IMPORT Std.Str AS Str;
IMPORT ML.Mat;;
IMPORT ML.Types;
IMPORT ML.Utils;
IMPORT STD;

lMatrix:={UNSIGNED id;REAL x;REAL y;};
// YinyangKMeans testing file: summer intern 2017
 

// DP100
dDocumentMatrix:=DATASET([
{1,2.4639,7.8579},
{2,0.5573,9.4681},
{3,4.6054,8.4723},
{4,1.24,7.3835},
{5,7.8253,4.8205},
{6,3.0965,3.4085},
{7,8.8631,1.4446},
{8,5.8085,9.1887},
{9,1.3813,0.515},
{10,2.7123,9.2429},
{11,6.786,4.9368},
{12,9.0227,5.8075},
{13,8.55,0.074},
{14,1.7074,3.9685},
{15,5.7943,3.4692},
{16,8.3931,8.5849},
{17,4.7333,5.3947},
{18,1.069,3.2497},
{19,9.3669,7.7855},
{20,2.3341,8.5196},
{21,0.5004,2.2394},
{22,6.5147,1.8744},
{23,5.1284,2.0043},
{24,3.555,1.3365},
{25,1.9224,8.0774},
{26,6.6664,9.9721},
{27,2.5007,5.2815},
{28,8.7526,6.6125},
{29,0.0898,3.9292},
{30,1.2544,9.5753},
{31,1.5462,8.4605},
{32,3.723,4.1098},
{33,9.8581,8.0831},
{34,4.0208,2.7462},
{35,4.6232,1.3271},
{36,1.5694,2.168},
{37,1.8174,4.779},
{38,9.2858,3.3175},
{39,7.1321,2.2322},
{40,2.9921,3.2818},
{41,7.0561,9.2796},
{42,1.4107,2.6271},
{43,5.1149,8.3582},
{44,6.8967,7.6558},
{45,0.0982,8.2855},
{46,1.065,4.9598},
{47,0.3701,3.7443},
{48,3.1341,8.8177},
{49,3.1314,7.3348},
{50,9.6476,3.3575},
{51,6.1636,5.3563},
{52,8.9044,7.8936},
{53,9.7695,9.6457},
{54,2.3383,2.229},
{55,5.9883,9.3733},
{56,9.3741,4.4313},
{57,8.4276,2.9337},
{58,8.2181,1.0951},
{59,3.2603,6.9417},
{60,3.0235,0.8046},
{61,1.0006,9.4768},
{62,8.5635,9.2097},
{63,5.903,7.6075},
{64,4.3534,7.5549},
{65,8.2062,3.453},
{66,9.0327,8.9012},
{67,8.077,8.6283},
{68,4.7475,5.5387},
{69,2.4441,7.106},
{70,8.1469,1.1593},
{71,5.0788,5.315},
{72,5.1421,9.8605},
{73,7.7034,2.019},
{74,3.5393,2.2992},
{75,2.804,1.3503},
{76,4.7581,2.2302},
{77,2.6552,1.7776},
{78,7.4403,5.5851},
{79,2.6909,9.7426},
{80,7.2932,5.4318},
{81,5.7443,4.3915},
{82,3.3988,9.8385},
{83,2.5105,3.6425},
{84,4.3386,4.9175},
{85,6.5916,5.7468},
{86,2.7913,7.4308},
{87,9.3152,5.4451},
{88,9.3501,3.9941},
{89,1.7224,4.6733},
{90,6.6617,1.6269},
{91,3.0622,1.9185},
{92,0.6733,2.4744},
{93,1.355,1.0267},
{94,3.75,9.499},
{95,7.2441,0.5949},
{96,3.3434,4.9163},
{97,8.7538,5.3958},
{98,7.4316,2.6315},
{99,3.6239,5.3696},
{100,3.2393,3.0533}
],lMatrix);

dCentroidMatrix:=DATASET([
{101,1,1},
{102,2,2},
{103,3,3},
{104,4,4}
],lMatrix);

dCentroidMatrix1:=DATASET([
{105,1,1},
{106,2,2}
],lMatrix);

ML.ToField(dDocumentMatrix,dDocuments);
ML.ToField(dCentroidMatrix,dCentroids);
ML.ToField(dCentroidMatrix1,dCentroids1);

OUTPUT( dDocuments, NAMED('dDocuments'));
OUTPUT( dCentroids, NAMED('dCentroids'));


SimpleDistances(DATASET(Types.NumericField) d01,DATASET(Types.NumericField) d02) := FUNCTION
dDistance_pwr := JOIN(d01, d02, LEFT.id != RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.id, SELF.y := RIGHT.id, SELF.value :=(LEFT.value - RIGHT.value) * (LEFT.value - RIGHT.value)), MANY LOOKUP); 
dDistance_sum := TABLE(dDistance_pwr,{x, y, val:=SUM(GROUP,value);},x,y, LOCAL);
dDist := PROJECT(dDistance_sum,TRANSFORM(Mat.Types.Element,SELF.value:=SQRT(LEFT.val),SELF := LEFT), LOCAL); 
RETURN dDist; 
END;

Drift(DATASET(Types.NumericField) d01,DATASET(Types.NumericField) d02) := FUNCTION
dDistance_pwr := JOIN(d01, d02, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(Types.NumericField, SELF.value :=(LEFT.value - RIGHT.value) * (LEFT.value - RIGHT.value), SELF := LEFT),LOOKUP); 
dDelta:= TABLE(dDistance_pwr,{id, val:=SQRT(SUM(GROUP,value));},id, LOCAL); 
RETURN dDelta; 
END;


dDistances := SimpleDistances(dDocuments,dCentroids);
// dDistance_pwr := JOIN(DISTRIBUTE(dDocuments,id), d02, LEFT.id != RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.id, SELF.y := RIGHT.id, SELF.value :=(LEFT.value - RIGHT.value) * (LEFT.value - RIGHT.value)), MANY LOOKUP); 
// OUTPUT( dDistance_pwr, NAMED('dDistance_pwr'));
// dDistance_sum := TABLE(dDistance_pwr,{x, y, val:=SUM(GROUP,value);},x,y, LOCAL);
// OUTPUT( dDistance_sum, NAMED('dDistance_sum'));
// dDistances := PROJECT(dDistance_sum,TRANSFORM(Mat.Types.Element,SELF.value:=SQRT(LEFT.val),SELF := LEFT), LOCAL);  
// OUTPUT(dDistances, NAMED('dDistance_sqrt'));
dUpperBound := DEDUP(SORT(dDistances, x, value, LOCAL), x, LOCAL);
OUTPUT(dUpperBound, NAMED('dUpperBound'));

// ds1:= topn(group(dDistances, x),2, value);
// output(ds1);

// dLowerBound := dedup(ds1, left.x=right.x, right);
// output(dLowerBound); 
   

		dDistancesSub := JOIN(dUpperBound,dDistances, LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y,RIGHT ONLY, LOCAL);
		OUTPUT(dDistancesSub, NAMED('dDistancesSub'));
		
    K := 4;
    t:=2;
		nt := 5;
		tConverge := 0.3;
    	
    KmeansDt := ML.kmeans.kmeans(dCentroids,dCentroids1,nt,tConverge);
		OUTPUT(KmeansDt.AllResults, NAMED('KmeansDt'));
  
		Gt := TABLE(KmeansDt.Allegiances(),{x,y},y,x);
		output_Gt := OUTPUT(Gt, NAMED('Gt'));
		OUTPUT(KmeansDt.Allegiances(), NAMED('group'));

    dGroupDistancesSub := SORT(JOIN(dDistancesSub, Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.y := RIGHT.y, SELF := LEFT), MANY LOOKUP),x, y, value, LOCAL);		
		dLowerBound := DEDUP(dGroupDistancesSub,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y, LOCAL);
    OUTPUT(dLowerBound, NAMED('dLowerBound'));
		
		
		dClusterCounts_ini:=TABLE(dUpperBound,{y;UNSIGNED c:=COUNT(GROUP);},y,FEW, LOCAL);
		dClustered_ini:=SORT(JOIN(dDocuments,dUpperBound,LEFT.id=RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;),LOCAL),id, number,LOCAL);
		dRolled_ini:=ROLLUP(dClustered_ini,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;),id,number,LOCAL);
		dJoined_ini:=JOIN(dRolled_ini,dClusterCounts_ini,LEFT.id=RIGHT.y,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value/RIGHT.c;SELF:=LEFT;),LOOKUP, LOCAL);		
		dPass_ini:=JOIN(dCentroids,TABLE(dJoined_ini,{id},id,LOCAL),LEFT.id=RIGHT.id,TRANSFORM(LEFT),LEFT ONLY,LOOKUP, LOCAL);
		dCentroid1 := dJoined_ini + dPass_ini;
		OUTPUT(dCentroid1, NAMED('dCentroid1'));
		
				
dDeltaC := Drift(dCentroids, dCentroid1);
OUTPUT(dDeltaC, NAMED('dDeltaC'));

			// deltaG1 := MAX(dDeltaC, val);
			// OUTPUT(deltaG1,NAMED('YinyangKMeans_deltaG1'));
			
		dGroupDeltaC :=JOIN(dDeltaC, Gt, LEFT.id = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.value := LEFT.val; SELF := RIGHT;));
			OUTPUT(dGroupDeltaC, NAMED('dGroupDeltaC'));
			dDeltaG := DEDUP(SORT(dGroupDeltaC,y,value, LOCAL),y,RIGHT, LOCAL);
			OUTPUT(dDeltaG, NAMED('dDeltaG'));
		//%%%%%%%%%%%%above this line all works well			
			dUbGroupFilter := JOIN(dUpperBound, dDeltaC, LEFT.y = RIGHT.id, TRANSFORM(Mat.Types.Element, SElF.value := LEFT.value + RIGHT.val; SELF := LEFT;), LOCAL);
      OUTPUT(dUbGroupFilter, NAMED('YinyangKMeans_dUbGroupFilter'));


//$$$$clusterpair 
ClusterPair:=RECORD
		Types.t_RecordID    id;
		Types.t_RecordID    clusterid;
		Types.t_FieldNumber number;
		Types.t_FieldReal   value01 := 0;
		Types.t_FieldReal   value02 := 0;
		Types.t_FieldReal   value03 := 0;
END;
      // dLbsGroupFilter := JOIN(dLowerBound, dDeltaG, LEFT.y = RIGHT.y , TRANSFORM(Mat.Types.Element, SELF.value := LEFT.value - RIGHT.value; SELF := LEFT),LOCAL);
      dLbsGroupFilter := JOIN(dLowerBound, dDeltaG, LEFT.y = RIGHT.y , TRANSFORM(ClusterPair, SELF.id := LEFT.x, SELF.Clusterid := RIGHT.x, SELF.number := LEFT.y, SELF.value01 := LEFT.value, SELF.value02 := RIGHT.value, SELF.value03 := LEFT.value - RIGHT.value;));			
      OUTPUT(dLbsGroupFilter, NAMED('YinyangKMeans_dLbsGroupFilter'));			
	//need to imporve :DP100: when the dataset is small, all the points go through gf at the first iter. so for small dataset how to smartly change the algorithm
			// gf_pass := JOIN(dUbGroupFilter, dLbsGroupFilter,LEFT.x = RIGHT.x AND RIGHT.value - LEFT.value03 < 0, TRANSFORM(RIGHT), LOCAL);
			
			gf_pass := JOIN(dUbGroupFilter, dLbsGroupFilter,LEFT.x = RIGHT.id AND RIGHT.value03 - LEFT.value < 0, TRANSFORM(ClusterPair, SELF.id := LEFT.x, SELF.Clusterid := LEFT.y, SELF.number := RIGHT.number, SELF.value01 := LEFT.value, SELF.value02 := RIGHT.value01, SELF.value03 :=RIGHT.value03 - LEFT.value;));
      OUTPUT(gf_pass, NAMED('YinyangKMeansv4_gf_pass'));	
						// gf_pass := JOIN(dUbGroupFilter, dLbsGroupFilter,LEFT.x = RIGHT.id , TRANSFORM(ClusterPair, SELF.id := LEFT.x, SELF.Clusterid := LEFT.y, SELF.number := RIGHT.number, SELF.value01 := LEFT.value, SELF.value02 := RIGHT.value01, SELF.value03 :=RIGHT.value03 - LEFT.value;));
      // OUTPUT(gf_pass, NAMED('YinyangKMeansv4_gf_pass'));	
			
			groupFilter := DEDUP(SORT(gf_pass,id),id);    
      OUTPUT(groupFilter, NAMED('YinyangKMeansv4_groupFilter'));

			// dLbs_lf := JOIN(dLowerBound, groupfilter, LEFT.x = RIGHT.x, TRANSFORM(LEFT), LOCAL);//looks ok
			// OUTPUT(dLbs_lf, NAMED('YinyangKMeansv4_dLbs_lf'));
			// lemma2_l_glb := DEDUP(SORT(dLbs_lf, x,value,LOCAL), x, LOCAL);//looks okay
			// lemma2_l_glb := DEDUP(SORT(groupFilter, id,value01,LOCAL), id, LOCAL);//looks okay
			lemma2_l_glb := DEDUP(SORT(dLowerBound, x, value), x);
			OUTPUT(lemma2_l_glb, NAMED('YinyangKMeansv4_lemma2_l_glb'));
					
      // lemma2_r :=JOIN(dLbs_lf, dGroupDeltaC, LEFT.y = RIGHT.y, TRANSFORM(Mat.Types.Element, SELF.y := RIGHT.x ,SELF.value := LEFT.value - RIGHT.value, SELF := LEFT));
      lemma2_r :=JOIN(gf_pass, dGroupDeltaC, LEFT.number = RIGHT.y, TRANSFORM(ClusterPair, SELF.Clusterid := RIGHT.x, SELF.number := RIGHT.y, SELF.value03 := LEFT.value02 - RIGHT.value, SELF := LEFT;));      
			OUTPUT(lemma2_r, NAMED('YinyangKMeansv4_llemma2_r'));  
			
      // lema2 := DEDUP(SORT(JOIN(lemma2_r, lemma2_l_glb, LEF T.x = RIGHT.x AND LEFT.value <= RIGHT.value, TRANSFORM(LEFT), LOOKUP),x),x);	      
      // lemma2 := DEDUP(SORT(JOIN(lemma2_r, lemma2_l_glb, LEFT.x = RIGHT.x AND LEFT.value03 <= RIGHT.value03, TRANSFORM(LEFT), LOCAL),x),x);	      
			// lemma2 := DEDUP(SORT(JOIN(lemma2_l_glb,lemma2_r,  LEFT.x = RIGHT.id AND LEFT.val > RIGHT.value02, TRANSFORM(Mat.Types.Element,SELF.x := RIGHT.id, SELF.y := RIGHT.clusterid,  SELF.value := LEFT.val ), LOCAL), x),x);	      
      lemma2 := JOIN(lemma2_l_glb,lemma2_r,  LEFT.x = RIGHT.id AND LEFT.value > RIGHT.value03, TRANSFORM(ClusterPair,SELF.value03 := LEFT.value - RIGHT.value03, SELF := RIGHT ));			
			OUTPUT(lemma2, NAMED('YinyangKMeansv4_lemma2'));
	
    	dLocalFilter_centroids := JOIN(dUpperBound,dCentroid1, LEFT.y = RIGHT.id,TRANSFORM(ClusterPair, SELF.id := LEFT.x, SELF.clusterid := LEFT.y, SELF.number := RIGHT.number, SELF.value01:= RIGHT.value, SELF.value02 := 0, SELF.value03 := 0));
			OUTPUT(dLocalFilter_centroids, NAMED('YinyangKMeansv4_dLocalFilter_centroids'));
		  
			dLocalFilter_points := JOIN(dLocalFilter_centroids,dDocuments, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number,TRANSFORM(ClusterPair, SELF.value02:= RIGHT.value, SELF.value03 := (LEFT.value01 -RIGHT.value)* (LEFT.value01 -RIGHT.value),SELF := LEFT ));
			OUTPUT(dLocalFilter_points, NAMED('YinyangKMeansv4_dLocalFilter_dLocalFilter_points'));
				
			// dDistancesLocalFilter := Distances(dLocalFilter,dCentroids);
			dDistancesLocalFilter_1 :=ROLLUP(SORT(dLocalFilter_points, id, clusterid), LEFT.id = RIGHT.id AND LEFT.clusterid = RIGHT.clusterid, TRANSFORM( ClusterPair,SELF.value03 := LEFT.value03 + RIGHT.value03, SELF := LEFT));
			OUTPUT(dDistancesLocalFilter_1, NAMED('YinyangKMeansv4_dDistancesLocalFilter_1'));
			
			dDistancesLocalFilter :=TABLE(dDistancesLocalFilter_1, {x := id, y := clusterid, val := SQRT(value03)});
      OUTPUT(dDistancesLocalFilter, NAMED('YinyangKMeansv4_dDistancesLocalFilter'));
			

						
			//new upperbound caculation
			
			dLocalFilter_centroids_change := JOIN(lemma2,dCentroid1, LEFT.clusterid = RIGHT.id,TRANSFORM(ClusterPair, SELF.number := RIGHT.number, SELF.value01:= RIGHT.value, SELF := LEFT ));
			OUTPUT(dLocalFilter_centroids_change, NAMED('YinyangKMeansv4_dLocalFilter_centroids_change'));
		  
			dLocalFilter_points_change := JOIN(dLocalFilter_centroids_change,dDocuments, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number,TRANSFORM(ClusterPair, SELF.value02:= RIGHT.value, SELF.value03 := (LEFT.value01 -RIGHT.value)* (LEFT.value01 -RIGHT.value),SELF := LEFT ));
			OUTPUT(dLocalFilter_points_change, NAMED('YinyangKMeansv4_dLocalFilter_dLocalFilter_points_change'));
				
			// dDistancesLocalFilter := Distances(dLocalFilter,dCentroids);
			dDistancesLocalFilter_1_change :=ROLLUP(SORT(dLocalFilter_points_change, id, clusterid), LEFT.id = RIGHT.id AND LEFT.clusterid = RIGHT.clusterid, TRANSFORM( ClusterPair,SELF.value03 := LEFT.value03 + RIGHT.value03, SELF := LEFT));
			OUTPUT(dDistancesLocalFilter_1_change, NAMED('YinyangKMeansv4_dDistancesLocalFilter_1_change'));
			
			dDistancesLocalFilter_change :=TABLE(dDistancesLocalFilter_1_change, {x := id, y := clusterid, val := SQRT(value03)});
      OUTPUT(dDistancesLocalFilter_change, NAMED('YinyangKMeansv4_dDistancesLocalFilter_change'));
			// dClosestLocalFilter := Closest(dDistancesLocalFilter);
			dClosestLocalFilter_change := DEDUP(SORT(dDistancesLocalFilter_change, x,val), LEFT.x = RIGHT.x);
			OUTPUT(dClosestLocalFilter_change, NAMED('YinyangKMeansv4_dClosestLocalFilter_change'));
			
			//above this line: dClosestLocalFilter is the new best centroids
			// LocalFilter := JOIN(dUpperbound, dClosestLocalFilter, LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y AND LEFT.value > RIGHT.value, TRANSFORM(Mat.Types.Element, SELF.value := RIGHT.value, SELF := RIGHT), LOCAL);
			LocalFilter := JOIN(dDistancesLocalFilter, dClosestLocalFilter_change, LEFT.x = RIGHT.x AND LEFT.val > RIGHT.val, TRANSFORM(Mat.Types.Element, SELF.value := RIGHT.val, SELF:=RIGHT));
      OUTPUT(LocalFilter, NAMED('YinyangKMeansv4_LocalFilter'));	
			// LocalFilter := JOIN(dDistancesLocalFilter, dClosestLocalFilter_change, LEFT.x = RIGHT.x , TRANSFORM(RIGHT));
      // OUTPUT(LocalFilter, NAMED('YinyangKMeansv4_LocalFilter'));
			// dUbUpdate_unchange := JOIN(dDistancesLocalFilter, LocalFilter, LEFT.x = RIGHT.x AND LEFT.y != RIGHT.y, TRANSFORM(LEFT));
			group_change := JOIN(LocalFilter,Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element, SELF.value := RIGHT.y,  SELF := LEFT));
      OUTPUT(group_change, NAMED('group_change'));			
			//get the new upperbound for all points
		  dUbUpdate := JOIN(dDistancesLocalFilter,LocalFilter,LEFT.x = RIGHT.x, TRANSFORM(Mat.Types.Element, SELF.y := IF( LEFT.x = RIGHT.x ,RIGHT.y,LEFT.y ),SELF.value := IF( LEFT.x = RIGHT.x,RIGHT.value,LEFT.val), SELF := LEFT;), LEFT OUTER );		
      OUTPUT(SORT(dUbUpdate,x), NAMED('YinyangKMeansv4_dUbUpdate'));
			
		
			// dLbsLocalFilter := JOIN(lemma2_r, localfilter,LEFT.id = RIGHT.x AND LEFT.clusterid != RIGHT.y, TRANSFORM(ClusterPair, SELF:=LEFT));
			dLbsLocalFilter := JOIN(lemma2_r, group_change,LEFT.id = RIGHT.x AND LEFT.number = RIGHT.value AND LEFT.clusterid != RIGHT.y, TRANSFORM(ClusterPair, SELF:=LEFT));
			OUTPUT(dLbsLocalFilter, NAMED('YinyangKMeansv4_dLbsLocalFilter'));	

		  dLbsLocalFilter_centroids_change := JOIN(dLbsLocalFilter,dCentroid1, LEFT.clusterid = RIGHT.id ,TRANSFORM(ClusterPair, SELF.value01:= RIGHT.value, SELF.value02 := RIGHT.number,  SELF := LEFT ));
			OUTPUT(dLbsLocalFilter_centroids_change, NAMED('YinyangKMeansv4_dLbsLocalFilter_centroids_change'));
		  
			dLbsLocalFilter_points_change := JOIN(dLbsLocalFilter_centroids_change,dDocuments, LEFT.id = RIGHT.id AND LEFT.value02 = RIGHT.number,TRANSFORM(ClusterPair, SELF.value03 := (LEFT.value01 -RIGHT.value)* (LEFT.value01 -RIGHT.value),SELF := LEFT ));
			OUTPUT(dLbsLocalFilter_points_change, NAMED('YinyangKMeansv4_dLocalFilter_dLbsLocalFilter_points_change'));
				
			// dDistancesLocalFilter := Distances(dLocalFilter,dCentroids);
			dDistancesLbsLocalFilter_1_change :=ROLLUP(SORT(dLbsLocalFilter_points_change, id, clusterid), LEFT.id = RIGHT.id AND LEFT.clusterid = RIGHT.clusterid, TRANSFORM( ClusterPair,SELF.value03 := LEFT.value03 + RIGHT.value03, SELF := LEFT));
			OUTPUT(dDistancesLbsLocalFilter_1_change, NAMED('YinyangKMeansv4_dDistancesLbsLocalFilter_1_change'));
			
			dDistancesLbsLocalFilter_change :=TABLE(dDistancesLbsLocalFilter_1_change, {x := id, y := number, val := SQRT(value03)});
      OUTPUT(dDistancesLbsLocalFilter_change, NAMED('YinyangKMeansv4_dDistancesLbsLocalFilter_change'));
			// dClosestLocalFilter := Closest(dDistancesLocalFilter);
			dClosestLbsLocalFilter_change := DEDUP(SORT(dDistancesLbsLocalFilter_change, x,y,val), LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y);
			OUTPUT(dClosestLbsLocalFilter_change, NAMED('YinyangKMeansv4_dClosestLbsLocalFilter_change'));
			
/*
			dGroupLbsLocalFilter := SORT(JOIN(dLbsLocalFilter, Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.y := RIGHT.y,SELF.value := LEFT.val, SELF := LEFT)),x, y, value);		
      OUTPUT(dGroupLbsLocalFilter, NAMED('YinyangKMeansv4_dGroupLbsLocalFilter'));		  
			dGroupLbsUpdate := DEDUP(dGroupLbsLocalFilter,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y);      
      OUTPUT(dGroupLbsUpdate, NAMED('YinyangKMeansv4_dGroupLbsUpdate'));
*/
			dLbsUpdate := JOIN(dLbsGroupFilter, dClosestLbsLocalFilter_change,LEFT.id = RIGHT.x AND LEFT.number = RIGHT.y, TRANSFORM(Mat.Types.Element, SELF.value := IF( LEFT.id = RIGHT.x AND LEFT.number = RIGHT.y,RIGHT.val,LEFT.value03), SELF.x := LEFT.id, SELF.y := LEFT.number), LEFT OUTER );
      OUTPUT(SORT(dLbsUpdate,x), NAMED('YinyangKMeansv4_dLbsUpdate'));
			
			
/*			
			//Get the lowerbound via the dDistancesLocalfilter 
      dLbsLocalFilter := JOIN(LocalFilter,dDistancesLocalFilter_change, LEFT.x = RIGHT.x AND LEFT.y != RIGHT.y, TRANSFORM(RIGHT),RIGHT ONLY);
      OUTPUT(dLbsLocalFilter, NAMED('YinyangKMeansv4_dLbsLocalFilter'));			
			dGroupLbsLocalFilter := SORT(JOIN(dLbsLocalFilter, Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.y := RIGHT.y,SELF.value := LEFT.val, SELF := LEFT)),x, y, value);		
      OUTPUT(dGroupLbsLocalFilter, NAMED('YinyangKMeansv4_dGroupLbsLocalFilter'));		  
			dGroupLbsUpdate := DEDUP(dGroupLbsLocalFilter,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y);      
      OUTPUT(dGroupLbsUpdate, NAMED('YinyangKMeansv4_dGroupLbsUpdate'));

			dLbsUpdate := JOIN(dLbsGroupFilter, dGroupLbsUpdate,LEFT.id = RIGHT.x AND LEFT.number = RIGHT.y, TRANSFORM(Mat.Types.Element, SELF.value := IF( LEFT.id = RIGHT.x AND LEFT.number = RIGHT.y,RIGHT.value,LEFT.value01), SELF.y := IF( LEFT.id = RIGHT.x AND LEFT.number = RIGHT.y, RIGHT.y, LEFT.number), SELF.x := LEFT.id;), LEFT OUTER );
      OUTPUT(SORT(dLbsUpdate,x), NAMED('YinyangKMeansv4_dLbsUpdate'));
*/			
/*			
	*/		
			
			
/*		
			dLocalFilter := JOIN(dDocuments,lemma2, LEFT.id = RIGHT.x,TRANSFORM(LEFT));
			action13 :=OUTPUT(dLocalFilter, NAMED('YinyangKMeansv4_dLocalFilter'));
					
			dDistancesLocalFilter := Distances(dLocalFilter,dCentroids);
			action14 :=OUTPUT(dDistancesLocalFilter, NAMED('YinyangKMeansv4_dDistancesLocalFilter'));
			dClosestLocalFilter := ML.Cluster.Closest(dDistancesLocalFilter);
			action15 :=OUTPUT(dClosestLocalFilter, NAMED('YinyangKMeansv4_dClosestLocalFilter'));
	    LocalFilter := JOIN(dClosestLocalFilter,dUpperbound, LEFT.x = RIGHT.x AND LEFT.y !=RIGHT.y, TRANSFORM(LEFT), LOOKUP, FEW);
      action16 :=OUTPUT(LocalFilter, NAMED('YinyangKMeansv4_LocalFilter'));			
			dUbLocalFilter := JOIN(dUpperbound, LocalFilter, LEFT.x = RIGHT.x, TRANSFORM(LEFT), LEFT ONLY);
			action17 :=OUTPUT(dUbLocalFilter, NAMED('YinyangKMeansv4_dUbLocalFilter'));	
			dMap := PROJECT(dUbLocalFilter, TRANSFORM(ClusterPair, SELF.id := LEFT.x; SELF.clusterid := LEFT.y;SELF.number := 0; SELF.value01 := LEFT.value; SELF.value02 := 0; SELF.value03 := 0;));		
      action18 :=OUTPUT(dMap, NAMED('YinyangKMeansv4_dMap'));			
			dMappedDistances := ML.Cluster.MappedDistances(dDocuments,dCentroids,fDist,dMap);
			action19 :=OUTPUT(dMappedDistances, NAMED('YinyangKMeansv4_dMappedDistances'));	
			
			dUbUpdate:= LocalFilter + dMappedDistances;
			action20 :=OUTPUT(SORT(dUbUpdate,x), NAMED('YinyangKMeansv4_dUbUpdate'));
			

			dLbsLocalFilter := JOIN(LocalFilter,dDistancesLocalFilter, LEFT.x = RIGHT.x , TRANSFORM(RIGHT),RIGHT ONLY);
			dGroupLbsLocalFilter := SORT(JOIN(dLbsLocalFilter, Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.y := RIGHT.y, SELF := LEFT)),x, y, value);		
		  dGroupLbsUpdate := DEDUP(dGroupLbsLocalFilter,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y);      


			dLbsUpdate := JOIN(dLbsGroupFilter, dGroupLbsUpdate,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y, TRANSFORM(Mat.Types.Element, SELF.value := IF( RIGHT.value = 0,LEFT.value, RIGHT.value), SELF.y := IF( RIGHT.value = 0,LEFT.y, RIGHT.y), SELF := LEFT;), LEFT OUTER, LOOKUP );
      action21 :=OUTPUT(SORT(dLbsUpdate,x), NAMED('YinyangKMeansv4_dLbsUpdate'));

*/

//%%%%%%%%%%% code added to v4_update
					//Calculate Vin: the number of data points who move into a new cluster					
					dClusterCountsVin:=TABLE(LocalFilter, {y; INTEGER c := COUNT(GROUP)},y);
					OUTPUT(dClusterCountsVin, NAMED('YinyangKMeansv4_dClusterCountsVin'));
					// Join closest to the document set and replace the id with the centriod id
					dClusteredVin := JOIN(dDocuments, LocalFilter, LEFT.id = RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;));
					OUTPUT(dClusteredVin, NAMED('YinyangKMeansv4_dClusteredVin'));
					// Now roll up on centroid ID, summing up the values for each axis
					dRolledVin:=ROLLUP(SORT(dClusteredVin,id, number),LEFT.number = RIGHT.number,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;));
					OUTPUT(dRolledVin, NAMED('YinyangKMeansv4_dRolledVin'));
					//Calculate Vout: the number of data points who move out a cluster
					VoutSet := JOIN(dUbGroupFilter, LocalFilter, LEFT.x = RIGHT.x, TRANSFORM(LEFT));
					OUTPUT(VoutSet, NAMED('YinyangKMeansv4_VoutSet'));
					dClusterCountsVout:=TABLE(VoutSet, {y; INTEGER c := COUNT(GROUP)},y);
					OUTPUT(dClusterCountsVout, NAMED('YinyangKMeansv4_dClusterCountsVout'));
					
					//Join closest to the document set and replace the id with the centriod id
					dClusteredVout := JOIN(dDocuments,VoutSet, LEFT.id = RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;));
					OUTPUT(dClusteredVout, NAMED('YinyangKMeansv4_dClusteredVout'));
					
					// Now roll up on centroid ID, summing up the values for each axis
					dRolledVout:=ROLLUP(SORT(dClusteredVout,id, number),LEFT.number = RIGHT.number,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;));
          OUTPUT(dRolledVout, NAMED('YinyangKMeansv4_dRolledVout'));
					//Update V to get the new cluster counts 
					
					dClusterCounts:=TABLE(dUpperBound,{y;UNSIGNED c:=COUNT(GROUP);},y,FEW);
					dClusterCountItr := PROJECT(dClusterCounts, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.y; SELF.y := LEFT.c; SELF.value := 0; ));
					dClusterCountsV :=JOIN(dClusterCountItr,dClusterCountsVin, LEFT.x = RIGHT.y, TRANSFORM(RECORDOF(LEFT), SELF.y := LEFT.y +RIGHT.c; SELF.value := LEFT.value + RIGHT.c; SELF := LEFT;), LEFT OUTER );
          OUTPUT(dClusterCountsV, NAMED('YinyangKMeansv4_dClusterCountsV'));					
					dClusterCountsUpdate:=JOIN(dClusterCountsV,dClusterCountsVout, LEFT.x = RIGHT.y, TRANSFORM(RECORDOF(LEFT), SELF.y := LEFT.y -RIGHT.c;SELF.value := LEFT.value -RIGHT.c; SELF := LEFT;), LEFT OUTER);
					OUTPUT(dClusterCountsUpdate, NAMED('YinyangKMeansv4_dClusterCountsUpdate'));
					//Join to cluster counts to calculate the new average on each axis
					dClustered :=JOIN(dCentroid1, dClusterCountItr, LEFT.id = RIGHT.x, TRANSFORM(RECORDOF(LEFT), SELF.value := LEFT.value * RIGHT.y; SELF := LEFT), LEFT OUTER);
          OUTPUT(dClustered, NAMED('YinyangKMeansv4_dClustered'));					
					//Add Vin 
					dRolled := JOIN(dClustered, dRolledVin, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(RECORDOF(LEFT), SELF.value := LEFT.value + RIGHT.value; SELF := LEFT), LEFT OUTER);
          OUTPUT(dRolled, NAMED('YinyangKMeansv4_dRolled'));					
					//Minus Vout
					dJoined:= JOIN(dRolled, dRolledVout, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(RECORDOF(LEFT), SELF.value := LEFT.value - RIGHT.value; SELF := LEFT), LEFT OUTER);
					OUTPUT(dJoined, NAMED('YinyangKMeansv4_dJoined'));	
					//New C
					// dCentroidOut:=JOIN(dJoined,dClusterCountsUpdate,LEFT.id=RIGHT.x,TRANSFORM(RECORDOF(LEFT),SELF.value:=(LEFT.value/RIGHT.y);SELF:=LEFT;), LEFT OUTER);
					dCentroid2:=JOIN(dJoined,dClusterCountsUpdate,LEFT.id=RIGHT.x,TRANSFORM(RECORDOF(LEFT),SELF.value:=(LEFT.value/RIGHT.y);SELF:=LEFT;), LEFT OUTER);
          OUTPUT(dCentroid2, NAMED('YinyangKMeansv4_dCentroid2'));	
					
				


/*		
			dLocalFilter_centroids := JOIN(lemma2,dCentroids, LEFT.Clusterid = RIGHT.id,TRANSFORM(ClusterPair, SELF.number := RIGHT.number, SELF.value01:= RIGHT.value, SELF := LEFT ), LOCAL);
			OUTPUT(dLocalFilter_centroids, NAMED('YinyangKMeansv4_dLocalFilter_centroids'));
		  
			dLocalFilter_points := JOIN(dLocalFilter_centroids,dDocuments, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number,TRANSFORM(ClusterPair, SELF.value02:= RIGHT.value, SELF.value03 := (LEFT.value01 -RIGHT.value)* (LEFT.value01 -RIGHT.value),SELF := LEFT ), LOCAL);
			OUTPUT(dLocalFilter_points, NAMED('YinyangKMeansv4_dLocalFilter_dLocalFilter_points'));
				
			// dDistancesLocalFilter := Distances(dLocalFilter,dCentroids);
			dDistancesLocalFilter_1 :=ROLLUP(SORT(dLocalFilter_points, id, clusterid, LOCAL), LEFT.id = RIGHT.id AND LEFT.clusterid = RIGHT.clusterid, TRANSFORM( ClusterPair,SELF.value03 := LEFT.value03 + RIGHT.value03, SELF := LEFT), LOCAL);
			OUTPUT(dDistancesLocalFilter_1, NAMED('YinyangKMeansv4_dDistancesLocalFilter_1'));
			dDistancesLocalFilter :=TABLE(dDistancesLocalFilter_1, {x := id, y := clusterid, val := SQRT(value03)},LOCAL);
      OUTPUT(dDistancesLocalFilter, NAMED('YinyangKMeansv4_dDistancesLocalFilter'));
			// dClosestLocalFilter := Closest(dDistancesLocalFilter);
			dClosestLocalFilter := TABLE(dDistancesLocalFilter, {x, y, value := MIN(GROUP,val)},x, y,LOCAL);
			OUTPUT(dClosestLocalFilter, NAMED('YinyangKMeansv4_dClosestLocalFilter'));

			
			// dClosestLocalFilter := PROJECT(dClosestLocalFilter_1, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.id, SELF.y := LEFT.clusterid, SELF.value := LEFT.value),LOCAL);
			// OUTPUT(dClosestLocalFilter, NAMED('YinyangKMeansv4_dClosestLocalFilter'));
			//%%%%%%%%%%%%localfilter is the new upperbound
	    LocalFilter := JOIN(dUpperbound, dClosestLocalFilter, LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y AND LEFT.value > RIGHT.value, TRANSFORM(Mat.Types.Element, SELF.value := RIGHT.value, SELF := RIGHT), LOCAL);
			// LocalFilter := JOIN(dClosestLocalFilter,dUpperbound, LEFT.x = RIGHT.x AND LEFT.y !=RIGHT.y, TRANSFORM(LEFT), LOCAL);
			// LocalFilter := PROJECT(dClosestLocalFilter(value01 > value), TRANSFORM(Mat.Types.Element, SELF.x := LEFT.id, SELF.y := LEFT.clusterid, SELF.value := LEFT.value),LOCAL);
      OUTPUT(LocalFilter, NAMED('YinyangKMeansv4_LocalFilter'));		

    	 
		 // updateset := JOIN(lemma, dDistancesLocalFilter, LEFT.id := RIGHT.x AND LEFT.clusterid = RIGHT.y AND RIGHT.value < LEFT.value01, TRANSFORM(ClusterPair, SELF.value02 := RIGHT.value, SELF.value03 := RIGHT.value, SELF := LEFT), LOCAL);
     // update_glb := TABLE(updateset, {x:= LEFT.id, y:= LEFT.clusterid, val := MIN(GROUP, value03)}, x,LOCAL);	   
		 // new_glb := JOIN(update_glb, lemma2_l_glb, LEFT.x = RIGHT.x, TRANSFORM(Mat.Types.Eletment, SELF.value := IF(RIGHT.value =0, RIGHT.val, LEFT.val)), RIGHT OUTER, LOCAL);
     
		 // update_lowerbound := JOIN(LocalFilter, dDistancesLocalFilter, LEFT.id := RIGHT.x AND LEFT.clusterid = RIGHT.y AND RIGHT.value < LEFT.value01, TRANSFORM(ClusterPair, SELF.value02 := RIGHT.value, SELF.value03 := RIGHT.value, SELF := LEFT), LOCAL);
    
			dUbLocalFilter := JOIN(dUpperbound, LocalFilter, LEFT.x = RIGHT.x, TRANSFORM(LEFT), LEFT ONLY, LOCAL);
			OUTPUT(dUbLocalFilter, NAMED('YinyangKMeansv4_dUbLocalFilter'));	
			
		
			dUbLocalFilter_centroids := JOIN(dUbLocalFilter,dCentroids, LEFT.y = RIGHT.id,TRANSFORM(ClusterPair, SELF.id := LEFT.x, SELF.clusterid := LEFT.y, SELF.number := RIGHT.number, SELF.value01:= RIGHT.value, SELF.value02 :=LEFT.value , SELF.value03 := 0), LOCAL);
			OUTPUT(dUbLocalFilter_centroids, NAMED('YinyangKMeansv4_dUbLocalFilter_centroids'));
		  
			dUbLocalFilter_points := JOIN(dUbLocalFilter_centroids,dDocuments, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number,TRANSFORM(ClusterPair, SELF.value02:= RIGHT.value, SELF.value03 := (LEFT.value01 -RIGHT.value)* (LEFT.value01 -RIGHT.value),SELF := LEFT ), LOCAL);
			OUTPUT(dUbLocalFilter_points , NAMED('YinyangKMeansv4_dUbLocalFilter_points'));
				
			// dDistancesLocalFilter := Distances(dLocalFilter,dCentroids);
			dDistancesdUbLocalFilter :=ROLLUP(SORT(dUbLocalFilter_points, id, clusterid,number, LOCAL), LEFT.id = RIGHT.id AND LEFT.clusterid = RIGHT.clusterid, TRANSFORM(ClusterPair, SELF.value03 := LEFT.value03 + RIGHT.value03, SELF := LEFT), LOCAL);
			OUTPUT(dDistancesdUbLocalFilter, NAMED('YinyangKMeansv4_dDistancesdUbLocalFilter'));
		
		
			dMappedDistances := TABLE(dDistancesdUbLocalFilter, {x := id, y:= clusterid, value := MIN(GROUP, SQRT(value03))}, id, clusterid,LOCAL);
			OUTPUT(dMappedDistances, NAMED('YinyangKMeansv4_dMappedDistances'));
		
		  // dMap := PROJECT(dUbLocalFilter, TRANSFORM(ClusterPair, SELF.id := LEFT.x; SELF.clusterid := LEFT.y;SELF.number := 0; SELF.value01 := LEFT.value; SELF.value02 := 0; SELF.value03 := 0;));		
      // OUTPUT(dMap, NAMED('YinyangKMeansv4_dMap'));			
			// dMappedDistances := MappedDistances(dDocuments,dCentroids,fDist,dMap);
			// OUTPUT(dMappedDistances, NAMED('YinyangKMeansv4_dMappedDistances'));	

			
			dMappedDistances1 := PROJECT(dMappedDistances, TRANSFORM(Mat.Types.Element, SELF := LEFT));
			dUbUpdate:= LocalFilter + dMappedDistances1;
			OUTPUT(SORT(dUbUpdate,x), NAMED('YinyangKMeansv4_dUbUpdate'));
			

			// dLbsLocalFilter := JOIN(LocalFilter,dDistancesdUbLocalFilter, LEFT.x = RIGHT.id , TRANSFORM(RIGHT),RIGHT ONLY);
			// dGroupLbsLocalFilter := SORT(JOIN(dLbsLocalFilter, Gt, LEFT.clusterid = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.y := RIGHT.y, SELF := LEFT)),x, y, value);		
		  // dGroupLbsUpdate := DEDUP(dGroupLbsLocalFilter,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y);      

			lemma := JOIN(LocalFilter,lemma2,  LEFT.x = RIGHT.id, TRANSFORM(RIGHT), LOCAL);
			updateset := JOIN(lemma, dDistancesLocalFilter, LEFT.id = RIGHT.x AND LEFT.clusterid = RIGHT.y AND RIGHT.val >= LEFT.value01 AND RIGHT.val < LEFT.value02, TRANSFORM(ClusterPair, SELF.value02 := RIGHT.val, SELF.value03 := RIGHT.val, SELF := LEFT), LOCAL);
			updateset1 := DEDUP(SORT(updateset, id, number,value02),LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number);
			dLbsUpdate := JOIN(dLbsGroupFilter, updateset1,LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.id, SELF.y := LEFT.number, SELF.value := IF( RIGHT.value01 = 0,LEFT.value01, RIGHT.value02)), LEFT OUTER, LOCAL);
      OUTPUT(SORT(dLbsUpdate,x), NAMED('YinyangKMeansv4_dLbsUpdate'));


//%%%%%%%%%%% code added to v4_update
					//Calculate Vin: the number of data points who move into a new cluster					
					dClusterCountsVin:=TABLE(LocalFilter, {y; INTEGER c := COUNT(GROUP)},y);
					OUTPUT(dClusterCountsVin, NAMED('YinyangKMeansv4_dClusterCountsVin'));
					// Join closest to the document set and replace the id with the centriod id
					dClusteredVin := JOIN(dDocuments, LocalFilter, LEFT.id = RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;));
					OUTPUT(dClusteredVin, NAMED('YinyangKMeansv4_dClusteredVin'));
					// Now roll up on centroid ID, summing up the values for each axis
					dRolledVin:=ROLLUP(SORT(dClusteredVin,id, number),LEFT.number = RIGHT.number,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;));
					OUTPUT(dRolledVin, NAMED('YinyangKMeansv4_dRolledVin'));
					//Calculate Vout: the number of data points who move out a cluster
					VoutSet := JOIN(dUbGroupFilter, LocalFilter, LEFT.x = RIGHT.x, TRANSFORM(LEFT));
					OUTPUT(VoutSet, NAMED('YinyangKMeansv4_VoutSet'));
					dClusterCountsVout:=TABLE(VoutSet, {y; INTEGER c := COUNT(GROUP)},y);
					OUTPUT(dClusterCountsVout, NAMED('YinyangKMeansv4_dClusterCountsVout'));
					
					//Join closest to the document set and replace the id with the centriod id
					dClusteredVout := JOIN(dDocuments,VoutSet, LEFT.id = RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;));
					OUTPUT(dClusteredVout, NAMED('YinyangKMeansv4_dClusteredVout'));
					
					// Now roll up on centroid ID, summing up the values for each axis
					dRolledVout:=ROLLUP(SORT(dClusteredVout,id, number),LEFT.number = RIGHT.number,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;));
          OUTPUT(dRolledVout, NAMED('YinyangKMeansv4_dRolledVout'));
					//Update V to get the new cluster counts 
					dClusterCounts := TABLE(KmeansDt.Allegiances(),{y;UNSIGNED c:=COUNT(GROUP);},y);
					dClusterCountItr :=PROJECT(dClusterCounts, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.y; SELF.y := LEFT.c; SELF.value := 0; ));
					dClusterCountsV :=JOIN(dClusterCountItr,dClusterCountsVin, LEFT.x = RIGHT.y, TRANSFORM(RECORDOF(LEFT), SELF.y := LEFT.y +RIGHT.c; SELF.value := LEFT.value + RIGHT.c; SELF := LEFT;), LEFT OUTER );
          OUTPUT(dClusterCountsV, NAMED('YinyangKMeansv4_dClusterCountsV'));					
					dClusterCountsUpdate:=JOIN(dClusterCountsV,dClusterCountsVout, LEFT.x = RIGHT.y, TRANSFORM(RECORDOF(LEFT), SELF.y := LEFT.y -RIGHT.c;SELF.value := LEFT.value -RIGHT.c; SELF := LEFT;), LEFT OUTER);
					OUTPUT(dClusterCountsUpdate, NAMED('YinyangKMeansv4_dClusterCountsUpdate'));
					//Join to cluster counts to calculate the new average on each axis
					dClustered :=JOIN(dCentroids, dClusterCountItr, LEFT.id = RIGHT.x, TRANSFORM(RECORDOF(LEFT), SELF.value := LEFT.value * RIGHT.y; SELF := LEFT), LEFT OUTER);
          OUTPUT(dClustered, NAMED('YinyangKMeansv4_dClustered'));					
					//Add Vin 
					dRolled := JOIN(dClustered, dRolledVin, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(RECORDOF(LEFT), SELF.value := LEFT.value + RIGHT.value; SELF := LEFT), LEFT OUTER);
          OUTPUT(dRolled, NAMED('YinyangKMeansv4_dRolled'));					
					//Minus Vout
					dJoined:= JOIN(dRolled, dRolledVout, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.number, TRANSFORM(RECORDOF(LEFT), SELF.value := LEFT.value - RIGHT.value; SELF := LEFT), LEFT OUTER);
					OUTPUT(dJoined, NAMED('YinyangKMeansv4_dJoined'));	
					//New C
					// dCentroidOut:=JOIN(dJoined,dClusterCountsUpdate,LEFT.id=RIGHT.x,TRANSFORM(RECORDOF(LEFT),SELF.value:=(LEFT.value/RIGHT.y);SELF:=LEFT;), LEFT OUTER);
					dCentroid2:=JOIN(dJoined,dClusterCountsUpdate,LEFT.id=RIGHT.x,TRANSFORM(RECORDOF(LEFT),SELF.value:=(LEFT.value/RIGHT.y);SELF:=LEFT;), LEFT OUTER);
          OUTPUT(dCentroid2, NAMED('YinyangKMeansv4_dCentroid2'));	

*/