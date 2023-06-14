
public class ComplexFlow {

    public static int test() {
        int i = 0;
        while(true) {
            i++;
            if (i > 1000) {
                break;
            }
            System.out.println(i);

            for (int v = 0; v < 100; v++) {

                if (i > 20 && v > 50) {
                    break;
                }

                System.out.println(v);

                if (i > 50 && v > 70) {
                    return 1;
                }
            }
        }

        return 0;
    }
}
