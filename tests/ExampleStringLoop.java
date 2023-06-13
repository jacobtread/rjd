public class ExampleStringLoop {

    public static String SOME_STATIC = "";
    public int[] exampleArray = new int[0];
    public int example = 0;

    public ExampleStringLoop() {
        
    }

    public ExampleStringLoop(int[] values) {
        this.exampleArray = values;
    }

    public void iterStr() {

        StringBuilder builder = new StringBuilder();

        int limit = 1000;
        for (int i = 0; i < limit; i++) {
            builder.append("This is a string");
            builder = builder.append('\n');
        }

        builder.reverse();

        System.out.println(limit);
        System.out.println(builder);

        int i = 0;
        while (i < limit) {
            builder.append("This is a string 1");
            builder.append('\n');
            i++;
        }

        System.out.println(builder);
    }

}
