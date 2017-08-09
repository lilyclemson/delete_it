//kegg record structure file: summer intern 2017
EXPORT KEGG := MODULE

lData := RECORD
STRING	Pathway;
INTEGER	Nodes ;
INTEGER	Edges	;
INTEGER	Connected_Components	;
INTEGER	Network_Diameter;
INTEGER	Network_Radius;
INTEGER	Shortest_Path	; 
REAL	Characteristic_Path ; 
REAL	Avg_num_Neighbours;	
INTEGER	Isolated_Nodes;	
INTEGER	Number_of_Self_Loops;	
INTEGER	Multi_edge_Node_Pair	;
REAL	NeighborhoodConnectivity;
REAL	Outdegree;
REAL Stress;
INTEGER	SelfLoops;
REAL	PartnerOfMultiEdgedNodePairs;	 
REAl	EdgeCount;
REAL	BetweennessCentrality; 
REAL	Indegree;	
REAL	Eccentricity;	 
REAL	ClosenessCentrality; 
REAL	AverageShortestPathLength;	
REAL	ClusteringCoefficient	;
END;

lData1 := RECORD
INTEGER Pathway;
INTEGER	Nodes ;
INTEGER	Edges	;
INTEGER	Connected_Components	;
INTEGER	Network_Diameter;
INTEGER	Network_Radius;
INTEGER	Shortest_Path	; 
REAL	Characteristic_Path ; 
REAL	Avg_num_Neighbours;	
INTEGER	Isolated_Nodes;	
INTEGER	Number_of_Self_Loops;	
INTEGER	Multi_edge_Node_Pair	;
REAL	NeighborhoodConnectivity;
REAL	Outdegree;
REAL Stress;
INTEGER	SelfLoops;
REAL	PartnerOfMultiEdgedNodePairs;	 
REAl	EdgeCount;
REAL	BetweennessCentrality; 
REAL	Indegree;	
REAL	Eccentricity;	 
REAL	ClosenessCentrality; 
REAL	AverageShortestPathLength;	
REAL	ClusteringCoefficient	;
END;


lKegg :=  RECORD
    INTEGER id;
    REAL field2;
    REAL field3;
    REAL field4;
    REAL field5;
    REAL field6;
    REAL field7;
    REAL field8;
    REAL field9;
    REAL field10;
    REAL field11;
    REAL field12;
    REAL field13;
    REAL field14;
    REAL field15;
    REAL field16;
    REAL field17;
    REAL field18;
    REAL field19;
    REAL field20;
    REAL field21;
    REAL field22;
    REAL field23;
    REAL field24;
    REAL field25;
    REAL field26;
    REAL field27;
    REAL field28;
    REAL field29;
END;		
lKegg changeFormat(lKegg ds, UNSIGNED c) := TRANSFORM
SELF.id := c;
SELF := ds;
END;


// dataTemp := DATASET('~::keggundirected.txt' ,lKegg,  CSV(HEADING(1)));
// dataTemp := DATASET('~JDH::yinyang::keggundirected.txt' ,lData,  CSV(HEADING(1)));
dataTemp := DATASET('~JDH::yinyang::keggundirected.txt' ,lKegg,  CSV(HEADING(1)));
EXPORT input := PROJECT(dataTemp,changeFormat(LEFT,COUNTER));
//EXPORT input := PROJECT(dataTemp,changeFormat(LEFT,COUNTER));
END;


