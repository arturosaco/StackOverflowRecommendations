package uk.ac.ucl.csml.ir;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import org.apache.commons.cli2.OptionException; 
import org.apache.mahout.cf.taste.common.NoSuchUserException;
import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.common.LongPrimitiveIterator;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.recommender.CachingRecommender;
import org.apache.mahout.cf.taste.impl.recommender.GenericItemBasedRecommender;
import org.apache.mahout.cf.taste.impl.similarity.*;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;

public class Recommend {
    
    final private static int NUM_REC = 1000;
    final private static String TRAIN_FILE = "datasets/train.csv";
    final private static String TEST_FILE = "datasets/test.csv";
    final private static String OUT_FILE = "datasets/recommendations.csv";
    
    public static void main(String... args) throws FileNotFoundException, TasteException, IOException, OptionException {
        
        // create data source (model) - from the csv file            
        File scoresFile = new File(TRAIN_FILE);                        
        DataModel model = new FileDataModel(scoresFile);
        
        System.out.println("Train file: " + TRAIN_FILE);
        System.out.println("Test file: " + TEST_FILE);
        
        // Item similarity
        //TanimotoCoefficientSimilarity itemSim = new TanimotoCoefficientSimilarity(model); //
        //PearsonCorrelationSimilarity itemSim = new PearsonCorrelationSimilarity(model); //
        LogLikelihoodSimilarity itemSim = new LogLikelihoodSimilarity(model); // 0.0007501265
        //UncenteredCosineSimilarity itemSim = new UncenteredCosineSimilarity(model); // 0.0007008744
        
        // create a simple recommender on our data
        CachingRecommender cachingRecommender = new CachingRecommender(new GenericItemBasedRecommender(model, itemSim));

        // for all users in the test set
        scoresFile = new File(TEST_FILE);                        
        model = new FileDataModel(scoresFile);
        
        File file = new File(OUT_FILE);
        if (!file.exists()) {
            file.createNewFile();
        }
        FileWriter fw = new FileWriter(file.getAbsoluteFile());
        BufferedWriter bw = new BufferedWriter(fw);
        bw.write("question_id,user_id,score\n");
            
        double meanRecs = 0;
        for (LongPrimitiveIterator it = model.getUserIDs(); it.hasNext();){
            long userId = it.nextLong();
            
            // get the recommendations for the user
            List<RecommendedItem> recommendations;
            try {
                recommendations = cachingRecommender.recommend(userId, NUM_REC);
                if (recommendations.isEmpty()){
                    continue;
                    //System.out.print("User ");
                    //System.out.print(userId);
                    //System.out.println(": no recommendations");
                }
            } catch (NoSuchUserException ex) {
                continue;
            }            
                            
            // print the list of recommendations for each 
            int numRecs = 0;
            /*System.out.print("User ");
            System.out.print(userId);
            System.out.println(":");
            for (RecommendedItem recommendedItem : recommendations) {
                System.out.print("\t");
                System.out.println(recommendedItem);
                numRecs++;
            }*/
                        
            for (RecommendedItem recommendedItem : recommendations) {
                //String line = userId + "," + recommendedItem.getItemID() + "," + recommendedItem.getValue();
                String line = userId + "," + recommendedItem.getItemID() + ","+ recommendedItem.getValue();
                //System.out.println(line);
                bw.write(line + "\n");
                numRecs++;
            }
            meanRecs += numRecs;
        }
        meanRecs /= model.getNumUsers();
        System.out.println("Average number of recommendations per question: " + meanRecs);
        System.out.println("Output file: " + OUT_FILE);
        bw.close();
    }
}
