/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package MC;

import org.apache.commons.math3.distribution.NormalDistribution;

/**
 *
 * @author manmo
 */
public class MC {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        double time1= System.currentTimeMillis();
        
         double K, S, T, sigma, roh, N, M, dt, nudt, sigsdt, lnS, sumCT, sumCT2, r, lnST, e, standard =1, normal=1, sample=1, ST, CT, power, SD, SE;
       
         S=100;
         K=120;
         T= 0.6;
         N=10; // number of steps
         M=10000; // number of simulations
         r=0.05;  
         sigma = 0.1; // volatility
         //dt= 0.01; 
         roh= 0;  // continous dividend yield
         power = r*T; //
                 
        dt= T/N;
        
        nudt= (r-roh-( (sigma*sigma)/2) )*dt;
        System.out.println(nudt);
        
        sigsdt= sigma*Math.sqrt(dt);
        
        lnS= Math.log(S);
        System.out.println(lnS);
        
        sumCT= 0;
        
        sumCT2=0;
  
        
        for(int j=0; j <M; j++)
        {
            lnST= lnS;
            for(int i=0; i<N; i++)
            {
                NormalDistribution v= new NormalDistribution();
                e= v.inverseCumulativeProbability(Math.random());
                lnST= lnST+ nudt+ sigsdt*e;
                
               // System.out.print(Math.exp(lnST)+",");
            
            }
            ST = Math.exp(lnST);
            CT = Math.max(0, (ST-K));
            
            //System.out.println("value of new Asset price is "+ST+" at the simulation number "+j);
            
            
            sumCT= sumCT+CT;
            
            sumCT2= sumCT2 + (CT*CT);
          // System.out.println();
        }
        
        double OV = (sumCT/M) * Math.exp(-(power));
        
        SD = Math.sqrt( (sumCT2 - (sumCT*sumCT)/M ) * ( Math.exp(-2*power))/ (M-1) );
        
        SE = SD/Math.sqrt(M);
        
        System.out.println("value of option is "+OV);
        
        System.out.println("value of SD is "+SD);
        
        System.out.println("value of SE is "+SE);
        
        
        double time2= System.currentTimeMillis();
        //System.out.println("time2 is "+time2);
        
        double time3= (time2-time1)/1000;
        //long timeSeconds = TimeUnit.MILLISECONDS.toSeconds(time3);
        System.out.println("Total time is "+time3+" MiliSeconds");
        
    }
    
}
