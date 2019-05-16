import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import io.dropwizard.auth.AuthFilter;

import javax.annotation.Priority;
import javax.ws.rs.InternalServerErrorException;
import javax.ws.rs.Priorities;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.core.SecurityContext;
import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Priority(Priorities.AUTHENTICATION)
public class GuardrailAllAuthFilter<C, T> extends AuthFilter<C, GuardrailAuthPrincipal<T>> {
    private final ImmutableList<AuthFilter<?, GuardrailAuthPrincipal<T>>> filters;

    public GuardrailAllAuthFilter(final ImmutableList<AuthFilter<?, GuardrailAuthPrincipal<T>>> filters) {
        if (filters.isEmpty()) {
            throw new IllegalArgumentException("Filters cannot be empty");
        }
        this.filters = filters;
    }

    @Override
    @SuppressWarnings("unchecked")
    public void filter(final ContainerRequestContext requestContext) throws IOException {
        final List<SecurityContext> securityContexts = new ArrayList<>(this.filters.size());
        for (final AuthFilter filter : this.filters) {
            final SecurityContext oldSecurityContext = requestContext.getSecurityContext();
            filter.filter(requestContext);
            if (oldSecurityContext != requestContext.getSecurityContext() && requestContext.getSecurityContext() != null) {
                securityContexts.add(requestContext.getSecurityContext());
            } else {
                throw new WebApplicationException(this.unauthorizedHandler.buildResponse(this.prefix, this.realm));
            }
        }

        final SecurityContext newSecurityContext = Lists.reverse(securityContexts)
                .stream()
                .map(securityContext -> {
                    try {
                        return (GuardrailAuthPrincipal<T>) securityContext.getUserPrincipal();
                    } catch (final ClassCastException e) {
                        throw new InternalServerErrorException("Security context principal is of the wrong type (" + securityContext.getUserPrincipal().getClass().getName() + ")");
                    }
                })
                .reduce((accum, next) -> {
                    next.setNext(accum);
                    return next;
                })
                .map(chainedPrincipal -> {
                    final boolean isSecure = securityContexts.stream().allMatch(SecurityContext::isSecure);
                    final String authenticationSchemes = securityContexts
                            .stream()
                            .map(SecurityContext::getAuthenticationScheme)
                            .collect(Collectors.joining(";"));

                    return new SecurityContext() {
                        @Override
                        public Principal getUserPrincipal() {
                            return chainedPrincipal;
                        }

                        @Override
                        public boolean isUserInRole(final String role) {
                            return securityContexts.stream().anyMatch(securityContext -> securityContext.isUserInRole(role));
                        }

                        @Override
                        public boolean isSecure() {
                            return isSecure;
                        }

                        @Override
                        public String getAuthenticationScheme() {
                            return authenticationSchemes;
                        }
                    };
                })
                .orElseThrow(() -> new WebApplicationException(this.unauthorizedHandler.buildResponse(this.prefix, this.realm)));

        requestContext.setSecurityContext(newSecurityContext);
    }
}
