import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Day1 {
    public static int matchingDigitsSum(String puzzleInput) {
        char[] inputDigits = puzzleInput.toCharArray();
        int sum = 0;
        int i, j;
        // check each adjacent pair of digits for equality
        for (int k = 0; k < inputDigits.length; k++) {
            i = Character.getNumericValue(inputDigits[k % inputDigits.length]);
            j = Character.getNumericValue(
                    inputDigits[(k + 1) % inputDigits.length]
            );
            if (i == j) {
                sum += i;
            }
        }
        return sum;
    }

    public static int halfwaySum(String puzzleInput) {
        char[] inputDigits = puzzleInput.toCharArray();
        int sum = 0;
        int i, j;
        // check digits halfway around the circular list for equality
        for (int k = 0; k < inputDigits.length; k++) {
            i = Character.getNumericValue(inputDigits[k % inputDigits.length]);
            j = Character.getNumericValue(
                    inputDigits[(k + inputDigits.length/2) % inputDigits.length]
            );
            if (i == j) {
                sum += i;
            }
        }
        return sum;
    }

    public static void main(String[] args) throws IOException {
        String contents = new String(Files.readAllBytes(Paths.get("1.txt")));
        contents = contents.trim();
        System.out.println(matchingDigitsSum(contents));
        System.out.println(halfwaySum(contents));
    }
}
