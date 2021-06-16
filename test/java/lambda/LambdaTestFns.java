package lambda;

import java.util.function.Predicate;

public class LambdaTestFns {
    public static String test(Predicate<String> predicate, String value) {
        if(predicate.test(value)) {
            return "Yes";
        } else {
            return "No";
        }
    }
}
