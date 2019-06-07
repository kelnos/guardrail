package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.{SupportDefinition, Target}
import com.twilio.guardrail.generators.syntax.Java.loadSupportDefinitionFromString

object JacksonHelpers {
  def jacksonSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "JacksonSupport",
    """
      import com.fasterxml.jackson.core.JsonGenerator;
      import com.fasterxml.jackson.core.JsonParser;
      import com.fasterxml.jackson.core.JsonProcessingException;
      import com.fasterxml.jackson.core.JsonToken;
      import com.fasterxml.jackson.core.type.TypeReference;
      import com.fasterxml.jackson.databind.DeserializationContext;
      import com.fasterxml.jackson.databind.JavaType;
      import com.fasterxml.jackson.databind.JsonDeserializer;
      import com.fasterxml.jackson.databind.JsonMappingException;
      import com.fasterxml.jackson.databind.JsonSerializer;
      import com.fasterxml.jackson.databind.ObjectMapper;
      import com.fasterxml.jackson.databind.SerializerProvider;
      import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
      import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
      import com.google.common.collect.Lists;
      import java.io.IOException;
      import java.lang.reflect.ParameterizedType;
      import java.lang.reflect.Type;
      import java.util.List;
      import java.util.Map;
      import java.util.Optional;
      import org.glassfish.hk2.utilities.reflection.ParameterizedTypeImpl;

      import static java.util.Objects.requireNonNull;

      public abstract class JacksonSupport {
          private static class PropertyValueSerializer<T> extends JsonSerializer<PropertyValue<T>> {
              @Override
              public void serialize(final PropertyValue<T> value, final JsonGenerator gen, final SerializerProvider serializers) throws IOException {
                  if (value.getValue().isPresent()) {
                      final Optional<T> ov = value.getValue().get();
                      if (ov.isPresent()) {
                          serializers.defaultSerializeValue(ov.get(), gen);
                      } else {
                          gen.writeNull();
                      }
                  }
              }
          }

          private static class PropertyValueDeserializer<T> extends JsonDeserializer<PropertyValue<T>> {
              private static class SettableTypeReference<T> extends TypeReference<T> {
                  private final Type type;

                  SettableTypeReference(final Type type) {
                      this.type = type;
                  }

                  @Override
                  public Type getType() {
                      return this.type;
                  }
              }

              @Override
              @SuppressWarnings("unchecked")
              public PropertyValue<T> deserialize(final JsonParser jp, final DeserializationContext ctxt) throws IOException, JsonProcessingException {
                  if (jp.getCurrentToken() == JsonToken.VALUE_NULL) {
                      jp.nextToken();
                      return new PropertyValue<>(Optional.empty());
                  } else {
                      final JavaType innerType = Optional.ofNullable(ctxt.getContextualType())
                              .flatMap(jt -> Optional.ofNullable(jt.containedType(0)))
                              .orElseThrow(() -> new JsonMappingException(jp, "Cannot find inner type for PropertyValue", jp.getCurrentLocation()));
                      final Class<?> innerClass = innerType.getRawClass();

                      if (Map.class.isAssignableFrom(innerClass)) {
                          final Class<?> keyClass = Optional.ofNullable(innerType.containedType(0))
                                  .map(JavaType::getRawClass)
                                  .orElseThrow(() -> new JsonMappingException(jp, "Cannot find key type for Map", jp.getCurrentLocation()));
                          if (!keyClass.equals(String.class)) {
                              throw new JsonMappingException(jp, "Only string keys are supported, but key type is " + keyClass.getName(), jp.getCurrentLocation());
                          }
                          final Class<?> valueClass = Optional.ofNullable(innerType.containedType(1))
                                  .map(JavaType::getRawClass)
                                  .orElseThrow(() -> new JsonMappingException(jp, "Cannot find value type for Map", jp.getCurrentLocation()));

                          final ParameterizedType type = new ParameterizedTypeImpl(Map.class, keyClass, valueClass);
                          final TypeReference<T> ref = new SettableTypeReference<>(type);
                          return (PropertyValue<T>) new PropertyValue<>(Optional.of(jp.readValueAs(ref)));

                          /*
                          final JsonNode mapRoot = jp.readValueAsTree();
                          final Iterator<String> fieldNames = mapRoot.fieldNames();
                          final Map<Object, Object> map = new HashMap<>(mapRoot.size());
                          while (fieldNames.hasNext()) {
                              final String fieldName = fieldNames.next();
                              map.put(fieldName, jp.getCodec().treeToValue(mapRoot.get(fieldName), valueClass));
                          }
                          return (PropertyValue<T>) new PropertyValue<>(map);
                           */
                      } else if (List.class.isAssignableFrom(innerClass)) {
                          final Class<?> elemClass = Optional.ofNullable(innerType.containedType(0))
                                  .map(JavaType::getRawClass)
                                  .orElseThrow(() -> new JsonMappingException(jp, "Cannot find element type for List", jp.getCurrentLocation()));
                          return (PropertyValue<T>) new PropertyValue<>(Lists.newArrayList(jp.readValuesAs(elemClass)));
                      } else {
                          return (PropertyValue<T>) new PropertyValue<>(jp.readValueAs(innerClass));
                      }
                  }
              }
          }

          public static class PropertyValue<T> {
              private final Optional<Optional<T>> value;

              public PropertyValue(final T value) {
                  this.value = Optional.of(Optional.of(requireNonNull(value, "value is required")));
              }

              public PropertyValue(final Optional<T> value) {
                  this.value = Optional.of(requireNonNull(value, "value is required"));
              }

              public PropertyValue() {
                  this.value = Optional.empty();
              }

              Optional<Optional<T>> getValue() {
                  return this.value;
              }
          }

          public static ObjectMapper configureObjectMapper(final ObjectMapper objectMapper) {
              objectMapper.registerModule(new JavaTimeModule());
              objectMapper.registerModule(new Jdk8Module());
              return objectMapper;
          }

          private JacksonSupport() {}
      }
    """
  )
}
