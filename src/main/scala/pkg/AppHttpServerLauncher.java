package pkg;

import io.activej.http.AsyncServlet;
import io.activej.inject.annotation.Provides;
import io.activej.inject.module.AbstractModule;
import io.activej.inject.module.Module;
import io.activej.worker.annotation.Worker;
import io.activej.worker.annotation.WorkerId;

public class AppHttpServerLauncher extends HttpServerLauncher {

    private final HttpService httpService = new HttpService();

    public AppHttpServerLauncher(int port, int workers) {
        super(port, workers);
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
                return httpService.servlet();
            }
        };
    }

}
