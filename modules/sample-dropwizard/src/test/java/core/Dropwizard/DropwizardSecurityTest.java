package core.Dropwizard;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import io.dropwizard.auth.AuthFilter;
import io.dropwizard.auth.Authenticator;
import io.dropwizard.auth.PolymorphicAuthDynamicFeature;
import io.dropwizard.auth.PolymorphicAuthValueFactoryProvider;
import io.dropwizard.auth.basic.BasicCredentialAuthFilter;
import io.dropwizard.auth.basic.BasicCredentials;
import io.dropwizard.auth.oauth.OAuthCredentialAuthFilter;
import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;
import security.server.dropwizard.ApiKeyHeaderApiKeyAuthFilter;
import security.server.dropwizard.ApiKeyHeaderAuthPrincipal;
import security.server.dropwizard.ApiKeyQueryApiKeyAuthFilter;
import security.server.dropwizard.ApiKeyQueryAuthPrincipal;
import security.server.dropwizard.Handler;
import security.server.dropwizard.HttpBasicAuthPrincipal;
import security.server.dropwizard.HttpBearerAuthPrincipal;
import security.server.dropwizard.Resource;

import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.util.Base64;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.assertj.core.api.Assertions.assertThat;

public class DropwizardSecurityTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final String API_KEY = "sekrit-api-key";
    private static final String BASIC_USERNAME = "some-user";
    private static final String BASIC_PASSWORD = "my-sekrit-password";
    private static final String BEARER_TOKEN = "my-sekrit-token";

    private static final Authenticator<BasicCredentials, HttpBasicAuthPrincipal<Object>> httpBasicAuthenticator =
            credentials -> Optional.of(new HttpBasicAuthPrincipal<>(credentials.getUsername(), credentials.getPassword()));

    private static final Authenticator<String, HttpBearerAuthPrincipal<String>> httpBearerAuthenticator =
            credentials -> Optional.of(new HttpBearerAuthPrincipal<>("HttpBearerAuth", credentials));

    private static final Authenticator<String, ApiKeyQueryAuthPrincipal<String>> apiKeyQueryAuthenticator =
            credentials -> Optional.of(new ApiKeyQueryAuthPrincipal<>("apiKey", credentials));

    private static final Authenticator<String, ApiKeyHeaderAuthPrincipal<String>> apiKeyHeaderAuthenticator =
            credentials -> Optional.of(new ApiKeyHeaderAuthPrincipal<>("x-api-key", credentials));

    private static final AuthFilter<String, ApiKeyQueryAuthPrincipal<String>> apiKeyQueryAuthFilter =
            new ApiKeyQueryApiKeyAuthFilter.Builder<String>()
                    .setAuthenticator(apiKeyQueryAuthenticator)
                    .setRealm("Some Realm")
                    .buildAuthFilter();

    private static final AuthFilter<String, ApiKeyHeaderAuthPrincipal<String>> apiKeyHeaderAuthFilter =
            new ApiKeyHeaderApiKeyAuthFilter.Builder<String>()
                    .setAuthenticator(apiKeyHeaderAuthenticator)
                    .setRealm("Some Realm")
                    .buildAuthFilter();

    private static final AuthFilter<BasicCredentials, HttpBasicAuthPrincipal<Object>> basicAuthFilter =
            new BasicCredentialAuthFilter.Builder<HttpBasicAuthPrincipal<Object>>()
                    .setAuthenticator(httpBasicAuthenticator)
                    .setRealm("Some Realm")
                    .buildAuthFilter();

    private static final AuthFilter<String, HttpBearerAuthPrincipal<String>> bearerAuthFilter =
            new OAuthCredentialAuthFilter.Builder<HttpBearerAuthPrincipal<String>>()
                    .setAuthenticator(httpBearerAuthenticator)
                    .setRealm("Some Realm")
                    .setPrefix("Bearer")
                    .buildAuthFilter();

    private static final Handler handler = new Handler() {
        @Override
        public CompletionStage<ApiKeyHeaderResponse> apiKeyHeader(List<Principal> authPrincipals, Optional<Integer> foo) {
            assertThat(authPrincipals.size()).isEqualTo(1);
            assertThat(authPrincipals.get(0)).isInstanceOf(ApiKeyHeaderAuthPrincipal.class);
            assertThat(((ApiKeyHeaderAuthPrincipal) authPrincipals.get(0)).getData()).isEqualTo(API_KEY);
            return completedFuture(ApiKeyHeaderResponse.Ok);
        }

        @Override
        public CompletionStage<ApiKeyQueryResponse> apiKeyQuery(List<Principal> authPrincipals, Optional<Integer> foo) {
            assertThat(authPrincipals.size()).isEqualTo(1);
            assertThat(authPrincipals.get(0)).isInstanceOf(ApiKeyQueryAuthPrincipal.class);
            assertThat(((ApiKeyQueryAuthPrincipal) authPrincipals.get(0)).getData()).isEqualTo(API_KEY);
            return completedFuture(ApiKeyQueryResponse.Ok);
        }

        @Override
        public CompletionStage<HttpBasicAuthResponse> httpBasicAuth(List<Principal> authPrincipals, Optional<Integer> foo) {
            assertThat(authPrincipals.size()).isEqualTo(1);
            assertThat(authPrincipals.get(0)).isInstanceOf(HttpBasicAuthPrincipal.class);
            assertThat(authPrincipals.get(0).getName()).isEqualTo(BASIC_USERNAME);
            assertThat(((HttpBasicAuthPrincipal) authPrincipals.get(0)).getData()).isEqualTo(BASIC_PASSWORD);
            return completedFuture(HttpBasicAuthResponse.Ok);
        }

        @Override
        public CompletionStage<HttpBearerAuthResponse> httpBearerAuth(List<Principal> authPrincipals, Optional<Integer> foo) {
            assertThat(authPrincipals.size()).isEqualTo(1);
            assertThat(authPrincipals.get(0)).isInstanceOf(HttpBearerAuthPrincipal.class);
            assertThat(((HttpBearerAuthPrincipal) authPrincipals.get(0)).getData()).isEqualTo(BEARER_TOKEN);
            return completedFuture(HttpBearerAuthResponse.Ok);
        }

        @Override
        public CompletionStage<MultipleAuthAndResponse> multipleAuthAnd(List<Principal> authPrincipals) {
            assertThat(authPrincipals.size()).isEqualTo(2);

            final List<ApiKeyHeaderAuthPrincipal> apiKeyPrincipals = authPrincipals
                    .stream()
                    .filter(ApiKeyHeaderAuthPrincipal.class::isInstance)
                    .map(ApiKeyHeaderAuthPrincipal.class::cast)
                    .collect(Collectors.toList());
            assertThat(apiKeyPrincipals.size()).isEqualTo(1);
            assertThat(apiKeyPrincipals.get(0).getData()).isEqualTo(API_KEY);


            final List<HttpBearerAuthPrincipal> bearerPrincipals = authPrincipals
                    .stream()
                    .filter(HttpBearerAuthPrincipal.class::isInstance)
                    .map(HttpBearerAuthPrincipal.class::cast)
                    .collect(Collectors.toList());
            assertThat(bearerPrincipals.size()).isEqualTo(1);
            assertThat(bearerPrincipals.get(0).getData()).isEqualTo(API_KEY);

            return completedFuture(MultipleAuthAndResponse.Ok);
        }
    };

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addProvider(new PolymorphicAuthDynamicFeature<>(ImmutableMap.of(
                    HttpBasicAuthPrincipal.class, basicAuthFilter,
                    HttpBearerAuthPrincipal.class, bearerAuthFilter,
                    ApiKeyQueryAuthPrincipal.class, apiKeyQueryAuthFilter,
                    ApiKeyHeaderAuthPrincipal.class, apiKeyHeaderAuthFilter
            )))
            .addProvider(new PolymorphicAuthValueFactoryProvider.Binder<>(ImmutableSet.of(
                    HttpBasicAuthPrincipal.class,
                    HttpBearerAuthPrincipal.class,
                    ApiKeyQueryAuthPrincipal.class,
                    ApiKeyHeaderAuthPrincipal.class
            )))
            .addResource(new Resource(handler))
            .build();

    @Test
    public void testApiKeyQueryAuth() {
        assertThat(
                resources
                        .target("/api-key-query")
                        .queryParam("ApiKey", API_KEY)
                        .request()
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testApiKeyHeaderAuth() {
        assertThat(
                resources
                        .target("/api-key-header")
                        .request()
                        .header("x-api-key", API_KEY)
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testHttpBasicAuth() {
        assertThat(
                resources
                        .target("/http-basic")
                        .request()
                        .header("authorization", createHttpBasicHeader(BASIC_USERNAME, BASIC_PASSWORD))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testHttpBearerAuth() {
        assertThat(
                resources
                        .target("/http-bearer")
                        .request()
                        .header("authorization", createHttpBearerHeader(BEARER_TOKEN))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testMultipleAnd() {
        assertThat(
                resources
                        .target("/multiple-and")
                        .request()
                        .header("x-api-key", API_KEY)
                        .header("authorization", createHttpBearerHeader(BEARER_TOKEN))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    private static String createHttpBasicHeader(final String username, final String password) {
        return "Basic " + Base64.getEncoder().encodeToString((username + ":" + password).getBytes(StandardCharsets.UTF_8));
    }

    private static String createHttpBearerHeader(final String token) {
        return "Bearer " + token;
    }
}
