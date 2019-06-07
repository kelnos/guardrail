package core.Dropwizard;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;
import org.mockito.stubbing.Answer;
import propertyPresence.client.dropwizard.JacksonSupport;
import propertyPresence.server.dropwizard.definitions.Foo;
import propertyPresence.server.dropwizard.propertyPresence.PropertyPresenceHandler;
import propertyPresence.server.dropwizard.propertyPresence.PropertyPresenceResource;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import java.util.Optional;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class DropwizardPropertyPresenceTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final String REQ = "req";
    private static final String REQ_NULL = "reqNull";
    private static final String OPT = "opt";
    private static final String OPT_NULL = "optNull";

    private static final ObjectMapper mapper = JacksonSupport.configureObjectMapper(new ObjectMapper());

    private static final PropertyPresenceHandler handler = mock(PropertyPresenceHandler.class);

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .setMapper(mapper)
            .addResource(new PropertyPresenceResource(handler))
            .build();

    @Test
    public void testSomeEmpty() throws ExecutionException, InterruptedException, JsonProcessingException {
        when(handler.foo(any())).thenAnswer((Answer<CompletionStage<PropertyPresenceHandler.FooResponse>>) invocation -> {
            final Foo foo = invocation.getArgument(0);
            assertThat(foo.getOptNull().isPresent()).isTrue();
            assertThat(foo.getOptNull().get().isPresent()).isFalse();
            return completedFuture(PropertyPresenceHandler.FooResponse.Ok(foo));
        });

        final String fooStr = mapper.writeValueAsString(new Foo.Builder(REQ)
                .withReqNull(REQ_NULL)
                .withOpt(OPT)
                .withOptNull(Optional.empty())
                .build()
        );
        sendRequest(fooStr);
    }

    private void sendRequest(final String json) {
        assertThat(resources
                .target("/foo")
                .request()
                .post(Entity.entity(json, MediaType.APPLICATION_JSON_TYPE))
                .getStatus()
        ).isEqualTo(200);
    }
}
