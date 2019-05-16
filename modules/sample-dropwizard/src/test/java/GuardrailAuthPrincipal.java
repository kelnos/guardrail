import java.security.Principal;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;

import static java.util.Objects.requireNonNull;

public abstract class GuardrailAuthPrincipal<T> implements Principal, Iterable<GuardrailAuthPrincipal<T>> {
    private final String name;
    private final T data;

    private Optional<GuardrailAuthPrincipal<T>> next;

    protected GuardrailAuthPrincipal(final String name, final T data) {
        this.name = requireNonNull(name);
        this.data = data;
    }

    public T getData() {
        return this.data;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final GuardrailAuthPrincipal<?> that = (GuardrailAuthPrincipal<?>) o;
        return this.name.equals(that.name) &&
                this.data.equals(that.data);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.name, this.data);
    }

    @Override
    public String toString() {
        return this.name;
    }

    public void setNext(final GuardrailAuthPrincipal<T> next) {
        this.next = Optional.of(next);
    }

    public Optional<GuardrailAuthPrincipal<T>> getNext() {
        return this.next;
    }

    @Override
    public Iterator<GuardrailAuthPrincipal<T>> iterator() {
        return new Iterator<GuardrailAuthPrincipal<T>>() {
            private Optional<GuardrailAuthPrincipal<T>> next = GuardrailAuthPrincipal.this.next;

            @Override
            public boolean hasNext() {
                return this.next.isPresent();
            }

            @Override
            public GuardrailAuthPrincipal<T> next() {
                if (this.next.isPresent()) {
                    final GuardrailAuthPrincipal<T> next = this.next.get();
                    this.next = next.next;
                    return next;
                } else {
                    throw new NoSuchElementException("GuardrailAuthPrincipal Iterator.next()");
                }
            }
        };
    }
}
