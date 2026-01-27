# Multi-stage build for Spring Boot application
FROM maven:3.9-eclipse-temurin-17 AS build

# Set working directory
WORKDIR /app

# Copy pom.xml first (for better layer caching)
# When building from project root: backend/pom.xml
# When building from backend dir: pom.xml
COPY backend/pom.xml pom.xml

# Download dependencies (this layer will be cached if pom.xml doesn't change)
# Retry up to 3 times in case of network issues (502, timeout, etc.)
RUN for i in 1 2 3; do \
        mvn dependency:go-offline -B && break || \
        (echo "Attempt $i failed, retrying..." && sleep 5); \
    done || echo "Warning: dependency:go-offline failed, will download during build"

# Copy source code
# When building from project root: backend/src
# When building from backend dir: src
COPY backend/src ./src

# Build the application
RUN mvn clean package -DskipTests

# Runtime stage
FROM eclipse-temurin:17-jre-alpine

# Install required tools for Fortran execution
# musl-dev and gcc are needed for linking Fortran programs
RUN apk add --no-cache \
    gfortran \
    gcc \
    musl-dev \
    python3 \
    py3-pip \
    bash \
    && rm -rf /var/cache/apk/*

# Install Python packages for plotting
# Use --break-system-packages flag for Alpine Linux (externally managed Python)
RUN pip3 install --no-cache-dir --break-system-packages numpy matplotlib

# Create app directory
WORKDIR /app

# Copy the JAR file from build stage
COPY --from=build /app/target/backend-0.0.1-SNAPSHOT.jar app.jar

# Copy fortran directory (when building from project root)
# For docker-compose: This is overridden by volume mount, so it's safe to include
COPY fortran /app/fortran

# Expose port
EXPOSE 8080

# Run the application
ENTRYPOINT ["java", "-jar", "app.jar"]

