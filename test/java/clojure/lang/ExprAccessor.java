package clojure.lang;

import java.util.concurrent.Callable;

public class ExprAccessor {
    public static Callable<Object> lambdaExpr(Class samClass, ISeq fnCode) {
        Compiler.Expr fnExpr = Compiler.FnExpr.parse(Compiler.C.EXPRESSION, fnCode, null);
        Compiler.LambdaExpr lambdaExpr = new Compiler.LambdaExpr(samClass, fnExpr);
        return new Callable<Object>() {
            @Override
            public Object call() throws Exception {
                return lambdaExpr.eval();
            }
        };
    }
}
