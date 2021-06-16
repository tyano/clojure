package lambda;

public interface SamInterfaceWithoutAnnotation {
    String hello();

    default String sayHello() {
        return hello();
    }

}
