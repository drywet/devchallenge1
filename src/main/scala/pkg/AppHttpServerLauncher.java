package pkg;

import io.activej.http.AsyncServlet;
import io.activej.inject.annotation.Provides;
import io.activej.inject.module.AbstractModule;
import io.activej.inject.module.Module;
import io.activej.worker.annotation.Worker;
import io.activej.worker.annotation.WorkerId;
import scala.Some;

public class AppHttpServerLauncher extends HttpServerLauncher implements AutoCloseable {

    private final Db db;
    private final HttpService httpService;

    public AppHttpServerLauncher(int port, int workers, String dbPath, boolean recreateDb) {
        super(port, workers);
        db = new Db(dbPath, recreateDb);
        httpService = new HttpService(Some.apply(db));
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

    @Override
    public void close() throws Exception {
        this.shutdown();
        db.close();
    }

}
