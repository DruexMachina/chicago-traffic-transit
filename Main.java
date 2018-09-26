import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Scanner;

public class Main {

	public Main() {
		// TODO Auto-generated constructor stub
	}

	public static void main(String[] args) throws IOException {
		long startTime = System.nanoTime();
		FileInputStream inputStream = null;
		Scanner sc = null;
		int linesRead = 0;

		HashMap<String, HashMap<String, Integer>> pickupNumRides = new HashMap<String, HashMap<String, Integer>>();
		HashMap<String, HashMap<String, Double>> dropoffTotalDistance = new HashMap<String, HashMap<String, Double>>();
		HashMap<String, HashMap<String, Integer>> dropoffNumRides = new HashMap<String, HashMap<String, Integer>>();

		try {
			inputStream = new FileInputStream("/home/hile/Documents/CSP571/chicago-traffic-transit/Taxi_Trips.csv");
			sc = new Scanner(inputStream, "UTF-8");

			//String header =
			sc.nextLine();
			/*String[] headArr = header.split(",");
		    int headInd = 0;
		    for(String line : headArr) {
		    	System.out.println(line+"; "+headInd++);
		    }
		    System.out.println();

		    String[] newLine = sc.nextLine().split(",");
		    System.out.println(newLine[2].substring(0, 10)+":");*/

			while (sc.hasNextLine()) {
				linesRead += 1;
				if(linesRead%100000 == 0)
					System.out.println(linesRead + " lines read");
				
				String line = sc.nextLine();
				String[] lineArr = line.split(",");
				if(lineArr[5].equals(""))
					continue;
				double tripLen = Double.parseDouble(lineArr[5]);
				String pickupCommArea = lineArr[8];
				String dropoffCommArea = lineArr[9];
				String date = lineArr[2].substring(0, 10);
				
				if(pickupCommArea.equals("") || tripLen == 0 || dropoffCommArea.equals(""))
					continue;
				
				if(!pickupNumRides.containsKey(pickupCommArea))
					pickupNumRides.put(pickupCommArea, new HashMap<String, Integer>());
				HashMap<String, Integer> pickupCommHolder = pickupNumRides.get(pickupCommArea);
					
				int pickupCount = pickupCommHolder.containsKey(date) ? pickupCommHolder.get(date) : 0;
				pickupCommHolder.put(date, pickupCount+1);


				if(!dropoffTotalDistance.containsKey(dropoffCommArea)) {
					dropoffTotalDistance.put(dropoffCommArea, new HashMap<String, Double>());
					dropoffNumRides.put(dropoffCommArea, new HashMap<String, Integer>());
				}
				HashMap<String, Double> distanceHolder = dropoffTotalDistance.get(dropoffCommArea);
				HashMap<String, Integer> numHolder = dropoffNumRides.get(dropoffCommArea);
					
				double currentDist = distanceHolder.containsKey(date) ? distanceHolder.get(date) : 0;
				distanceHolder.put(date, currentDist+tripLen);
					
				int dropoffCount = numHolder.containsKey(date) ? numHolder.get(date) : 0;
				numHolder.put(date, dropoffCount+1);
				
				//if(linesRead == 1000000)
				//	break;
			}
			// note that Scanner suppresses exceptions
			if (sc.ioException() != null) {
				throw sc.ioException();
			}
		} finally {
			if (inputStream != null) {
				inputStream.close();
			}
			if (sc != null) {
				sc.close();
			}
		}
		System.out.println("Reading complete!");
		
		PrintWriter pw1 = new PrintWriter(new File("/home/hile/Documents/CSP571/chicago-traffic-transit/taxi-pickups.csv"));
		
		for(String pickupCommArea : pickupNumRides.keySet()) {
			HashMap<String, Integer> commHolder = pickupNumRides.get(pickupCommArea);
			for(String date : commHolder.keySet()) {
				pw1.write(pickupCommArea+","+date+","+commHolder.get(date)+"\n");
			}
		}		
		pw1.close();
		System.out.println("Writing of file 1 complete!");
		
		PrintWriter pw2 = new PrintWriter(new File("/home/hile/Documents/CSP571/chicago-traffic-transit/taxi-dropoffs.csv"));
		for(String dropoffCommArea : dropoffNumRides.keySet()) {
			HashMap<String, Integer> numHolder = dropoffNumRides.get(dropoffCommArea);
			HashMap<String, Double> distHolder = dropoffTotalDistance.get(dropoffCommArea);
			for(String date : distHolder.keySet()) {
				double avgDist = distHolder.get(date)/numHolder.get(date);
				pw2.write(dropoffCommArea+","+date+","+numHolder.get(date)+","+avgDist+"\n");
			}
		}		
		pw2.close();
		
		System.out.println("Writing of file 2 complete!");
		
		long endTime = System.nanoTime();
		System.out.println((endTime - startTime)/1000000000/60 + " minutes (about); total");
	}

}
