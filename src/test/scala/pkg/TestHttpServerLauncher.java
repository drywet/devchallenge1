package pkg;

import io.activej.http.AsyncServlet;
import io.activej.http.HttpResponse;
import io.activej.inject.annotation.Provides;
import io.activej.inject.module.AbstractModule;
import io.activej.inject.module.Module;
import io.activej.worker.annotation.Worker;
import io.activej.worker.annotation.WorkerId;

public class TestHttpServerLauncher extends HttpServerLauncher implements AutoCloseable {

    private final long requestDelayMillis;

    public TestHttpServerLauncher(int port, int workers, long requestDelayMillis) {
        super(port, workers);
        this.requestDelayMillis = requestDelayMillis;
    }

    /**
     * getBusinessLogicModule is called only once
     */
    @Override
    protected Module getBusinessLogicModule() {
        return new AbstractModule() {
            @Provides
            @Worker
            AsyncServlet servlet(@WorkerId int workerId) {
                return request -> {
                    Thread.sleep(requestDelayMillis);
                    return HttpResponse.ok200().withPlainText("Hello, world! #" + workerId);
                };
            }
        };
    }

    @Override
    public void close() throws Exception {
        this.shutdown();
    }

}
