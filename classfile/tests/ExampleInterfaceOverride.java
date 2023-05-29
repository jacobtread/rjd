public class ExampleInterfaceOverride {

  
    public interface DoThing {
        public void doThing();
    }

    public class Test implements DoThing {
        @Override 
        public void doThing() {
            System.out.println("Didthing");
        }
    }
}
