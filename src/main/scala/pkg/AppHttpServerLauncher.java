package pkg;

import io.activej.http.AsyncHttpClient;
import io.activej.http.AsyncServlet;
import io.activej.inject.annotation.Inject;
import io.activej.inject.annotation.Provides;
import io.activej.inject.module.AbstractModule;
import io.activej.inject.module.Module;
import io.activej.worker.annotation.Worker;
import io.activej.worker.annotation.WorkerId;
import scala.Some;

@SuppressWarnings("unused")
public class AppHttpServerLauncher extends HttpServerLauncher implements AutoCloseable {

    private final String dbPath;
    private final boolean recreateDb;

    @Inject
    Db db;

    public AppHttpServerLauncher(int port, int workers, String dbPath, boolean recreateDb) {
        super(port, workers);
        this.dbPath = dbPath;
        this.recreateDb = recreateDb;
    }

    @Provides
    Db db() {
        return new Db(dbPath, recreateDb);
    }

    @Provides
    HttpService httpService(Db db, AsyncHttpClient httpClient) {
        return new HttpService(Some.apply(db), httpClient);
    }

    /**
     * getBusinessLogicModule is called only once
     */
    @Override
    protected Module getBusinessLogicModule() {
        return new AbstractModule() {
            @Provides
            @Worker
            AsyncServlet servlet(@WorkerId int workerId, HttpService httpService) {
                return httpService.servlet();
            }
        };
    }

    @Override
    public void close() {
        this.shutdown();
        db.close();
    }

}
