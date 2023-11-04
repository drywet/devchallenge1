package pkg;

import io.activej.config.Config;
import io.activej.config.ConfigModule;
import io.activej.eventloop.Eventloop;
import io.activej.eventloop.inspector.ThrottlingController;
import io.activej.http.AsyncHttpClient;
import io.activej.http.AsyncHttpServer;
import io.activej.http.AsyncServlet;
import io.activej.inject.annotation.Inject;
import io.activej.inject.annotation.Provides;
import io.activej.inject.binding.OptionalDependency;
import io.activej.inject.module.Module;
import io.activej.launcher.Launcher;
import io.activej.net.PrimaryServer;
import io.activej.service.ServiceGraphModule;
import io.activej.worker.WorkerPool;
import io.activej.worker.WorkerPoolModule;
import io.activej.worker.WorkerPools;
import io.activej.worker.annotation.Worker;

import javax.net.ssl.SSLContext;
import java.net.InetSocketAddress;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import static io.activej.config.Config.ofSystemProperties;
import static io.activej.config.converter.ConfigConverters.ofInetSocketAddress;
import static io.activej.config.converter.ConfigConverters.ofInteger;
import static io.activej.inject.module.Modules.combine;
import static io.activej.launchers.initializers.Initializers.*;

@SuppressWarnings({"WeakerAccess", "unused"})
public abstract class HttpServerLauncher extends Launcher {

    protected final int port;
    protected final int workers;

    public HttpServerLauncher(int port, int workers) {
        this.port = port;
        this.workers = workers;
    }

    @Inject
    PrimaryServer primaryServer;

    @Provides
    Eventloop primaryEventloop(Config config) {
        return Eventloop.create()
                .withInitializer(ofEventloop(config.getChild("eventloop.primary")));
    }

    @Provides
    @Worker
    Eventloop workerEventloop(Config config, OptionalDependency<ThrottlingController> throttlingController) {
        return Eventloop.create()
                .withInitializer(ofEventloop(config.getChild("eventloop.worker")))
                .withInitializer(eventloop -> eventloop.withInspector(throttlingController.orElse(null)));
    }

    @Provides
    WorkerPool workerPool(WorkerPools workerPools, Config config) {
        return workerPools.createPool(config.get(ofInteger(), "workers", workers));
    }

    @Provides
    PrimaryServer primaryServer(Eventloop primaryEventloop, WorkerPool.Instances<AsyncHttpServer> workerServers, Config config) {
        return PrimaryServer.create(primaryEventloop, workerServers.getList())
                .withInitializer(ofPrimaryServer(config.getChild("http")));
    }

    @Provides
    @Worker
    AsyncHttpServer workerServer(Eventloop eventloop, AsyncServlet servlet, Config config) {
        return AsyncHttpServer.create(eventloop, servlet)
                .withInitializer(ofHttpWorker(config.getChild("http")));
    }

    @Provides
    Config config() {
        return Config.create()
                .with("http.listenAddresses", Config.ofValue(ofInetSocketAddress(), new InetSocketAddress(port)))
                .with("workers", "" + workers)
                .overrideWith(ofSystemProperties("config"));
    }

    @Provides
    SSLContext sslContext() {
        try {
            return SSLContext.getDefault();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    @Provides
    Executor executor() {
        return Executors.newCachedThreadPool();
    }

    @Provides
    AsyncHttpClient client(Eventloop eventloop, SSLContext sslContext, Executor executor) {
        return AsyncHttpClient.create(eventloop)
                .withSslEnabled(sslContext, executor);
    }

    @Override
    protected final Module getModule() {
        return combine(
                ServiceGraphModule.create(),
                WorkerPoolModule.create(),
                ConfigModule.create()
                        .withEffectiveConfigLogger(),
                getBusinessLogicModule()
        );
    }

    protected Module getBusinessLogicModule() {
        return Module.empty();
    }

    @Override
    protected void run() throws Exception {
        logger.info("HTTP Server is listening on {}",
                primaryServer.getListenAddresses().stream().map(address -> "http://" + ("0.0.0.0".equals(address.getHostName()) ? "localhost" : address.getHostName()) + (address.getPort() != 80 ? ":" + address.getPort() : "") + "/")
        );
        awaitShutdown();
    }

}
