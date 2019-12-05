package core.Dropwizard;

import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import parameters.server.dropwizard.parameters.ParametersHandler;
import parameters.server.dropwizard.parameters.ParametersResource;

import javax.ws.rs.core.Response;
import java.util.concurrent.CompletableFuture;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DropwizardParametersTest {
    private static final ParametersHandler handler = mock(ParametersHandler.class);

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addResource(new ParametersResource(handler))
            .build();

    @Before
    public void before() {
        reset(handler);
        when(handler.getFoo(anyString(), anyString())).thenReturn(CompletableFuture.completedFuture(ParametersHandler.GetFooResponse.Ok));
    }

    @Test
    public void validParameters() {
        final Response response = resources
                .target("/foo")
                .queryParam("refParam", "bar")
                .queryParam("inlineParam", "baz")
                .request()
                .get();
        assertEquals(200, response.getStatus());
        verify(handler, times(1)).getFoo(eq("bar"), eq("baz"));
    }

    @Test
    public void missingRefParam() {
        final Response response = resources
                .target("/foo")
                .queryParam("inlineParam", "baz")
                .request()
                .get();
        assertEquals(400, response.getStatus());
        verify(handler, never()).getFoo(anyString(), anyString());
    }

    @Test
    public void missingInlineParam() {
        final Response response = resources
                .target("/foo")
                .queryParam("refParam", "bar")
                .request()
                .get();
        assertEquals(400, response.getStatus());
        verify(handler, never()).getFoo(anyString(), anyString());
    }
}
