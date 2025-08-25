# Protagentic Examples

This document provides detailed examples of using Protagentic for different types of software projects.

## Table of Contents

- [Web Application Examples](#web-application-examples)
- [API Development Examples](#api-development-examples)
- [Mobile App Examples](#mobile-app-examples)
- [Library/Framework Examples](#libraryframework-examples)
- [DevOps/Infrastructure Examples](#devopsinfrastructure-examples)
- [Data Processing Examples](#data-processing-examples)

## Web Application Examples

### Example 1: User Dashboard Feature

**Scenario:** Adding a user dashboard to an existing web application.

#### Step 1: Create Spec
```
M-x protagentic-create-spec RET
Spec name: user-dashboard RET
Describe the 'user-dashboard' feature: A personalized dashboard showing user activity, recent items, and quick actions RET
```

#### Generated Requirements (excerpt)
```markdown
### Requirement 1
**User Story:** As a logged-in user, I want to see my recent activity, so that I can quickly resume my work

#### Acceptance Criteria
1. WHEN user accesses dashboard THEN system SHALL display last 10 activities
2. WHEN user has no recent activity THEN system SHALL display welcome message
3. WHEN activity item is clicked THEN system SHALL navigate to relevant page
```

#### Generated Design (excerpt)
```markdown
#### Dashboard Component
Manages the main dashboard layout and data aggregation

**Key Responsibilities:**
- Activity feed aggregation
- Widget layout management
- Real-time data updates
- User preference handling

#### Activity Model
```
Activity {
  id: identifier
  user_id: foreign_key
  action_type: string
  resource_id: identifier
  timestamp: datetime
  metadata: json
}
```
```

#### Generated Tasks (excerpt)
```markdown
- [ ] 1. Set up dashboard component structure
  - Create React dashboard component with routing
  - Set up state management for dashboard data
  - _Requirements: 1.1, 2.1_

- [ ] 2. Implement activity tracking system
  - [ ] 2.1 Create Activity model with validation
    - Define Activity schema with proper indexing
    - Add user relationship and activity types
    - _Requirements: 1.1, 1.2_
  
  - [ ] 2.2 Build activity logging middleware
    - Create middleware to capture user actions
    - Add filtering for relevant activities
    - _Requirements: 1.1_
```

### Example 2: Shopping Cart Feature

**Scenario:** E-commerce shopping cart with persistence and checkout.

#### Requirements Refinement Session
```
M-x protagentic-refine-requirements RET

Quality Score: 65/100

Improvement Suggestions:
• Add acceptance criteria using WHEN/THEN or IF/THEN format
• Consider adding more requirements to cover edge cases

Choose action: (1) Add requirement (2) Improve existing (3) Add criteria (4) Finalize: 1

User role: customer
What feature do they want? add items to cart and persist across sessions
What benefit/value does it provide? can build up purchases over time without losing items
How many acceptance criteria? 3
Acceptance criterion 1: WHEN customer adds item to cart THEN system SHALL save cart state to database
Acceptance criterion 2: WHEN customer returns after session timeout THEN system SHALL restore previous cart contents
Acceptance criterion 3: WHEN cart item is no longer available THEN system SHALL notify customer and offer alternatives
```

## API Development Examples

### Example 3: REST API Rate Limiting

**Scenario:** Adding rate limiting to protect API endpoints.

#### Generated Requirements
```markdown
### Requirement 1
**User Story:** As an API provider, I want to limit request rates per client, so that I can prevent abuse and ensure service availability

#### Acceptance Criteria
1. WHEN client exceeds rate limit THEN system SHALL return 429 Too Many Requests
2. WHEN client is within limits THEN system SHALL process request normally
3. WHEN rate limit window resets THEN system SHALL allow new requests

### Requirement 2
**User Story:** As an API client, I want to know my rate limit status, so that I can adjust my request patterns

#### Acceptance Criteria
1. WHEN making API request THEN system SHALL include rate limit headers in response
2. WHEN approaching rate limit THEN system SHALL include time until reset
3. IF rate limit exceeded THEN system SHALL include retry-after header
```

#### Generated Design
```markdown
#### RateLimiter Component
Manages request rate tracking and enforcement

**Key Responsibilities:**
- Request counting per client/endpoint
- Rate limit rule evaluation
- Response header generation
- Cleanup of expired counters

#### RateLimit Model
```
RateLimit {
  client_id: string
  endpoint: string
  request_count: integer
  window_start: timestamp
  limit: integer
  window_duration: integer
}
```

### Technology Stack
- **Backend:** Node.js with Express middleware
- **Storage:** Redis for fast counter operations
- **Monitoring:** Prometheus metrics for rate limit violations
```

#### Generated Tasks
```markdown
- [ ] 1. Implement rate limiting middleware
  - Create Express middleware for rate limit checking
  - Add Redis client for distributed rate limiting
  - Write unit tests for rate limit logic
  - _Requirements: 1.1, 1.2, 1.3_

- [ ] 2. Add rate limit configuration system
  - [ ] 2.1 Create rate limit rules engine
    - Define configurable limits per endpoint/client
    - Add support for different time windows
    - _Requirements: 1.1_
  
  - [ ] 2.2 Implement rate limit headers
    - Add X-RateLimit-Limit header
    - Include X-RateLimit-Remaining counter
    - Add X-RateLimit-Reset timestamp
    - _Requirements: 2.1, 2.2_
```

## Mobile App Examples

### Example 4: Offline Data Sync

**Scenario:** Mobile app feature for offline data synchronization.

#### Generated Requirements
```markdown
### Requirement 1
**User Story:** As a mobile user, I want to access my data when offline, so that I can continue working without internet connection

#### Acceptance Criteria
1. WHEN app goes offline THEN system SHALL switch to local data storage
2. WHEN user modifies data offline THEN system SHALL queue changes for sync
3. WHEN connection restored THEN system SHALL automatically sync pending changes

### Requirement 2
**User Story:** As a mobile user, I want to resolve sync conflicts, so that I don't lose important changes

#### Acceptance Criteria
1. WHEN sync conflict detected THEN system SHALL present conflict resolution options
2. WHEN user chooses resolution THEN system SHALL apply changes and mark conflict resolved
3. IF automatic resolution possible THEN system SHALL resolve without user intervention
```

#### Generated Design
```markdown
#### SyncManager Component
Handles offline/online state and data synchronization

**Key Responsibilities:**
- Network state monitoring
- Change queue management
- Conflict detection and resolution
- Background sync scheduling

#### SyncQueue Model
```
SyncQueue {
  id: identifier
  operation_type: enum(create, update, delete)
  resource_type: string
  resource_id: identifier
  data: json
  timestamp: datetime
  retry_count: integer
  status: enum(pending, syncing, completed, failed)
}
```
```

## Library/Framework Examples

### Example 5: Validation Library

**Scenario:** Creating a data validation library with fluent API.

#### Generated Requirements
```markdown
### Requirement 1
**User Story:** As a developer, I want to define validation rules with a fluent API, so that I can write readable validation code

#### Acceptance Criteria
1. WHEN defining validation rules THEN system SHALL provide chainable methods
2. WHEN validation fails THEN system SHALL return detailed error messages
3. WHEN validation passes THEN system SHALL return success indicator

### Requirement 2
**User Story:** As a developer, I want custom validation rules, so that I can validate domain-specific data

#### Acceptance Criteria
1. WHEN registering custom validator THEN system SHALL accept validator function
2. WHEN using custom validator THEN system SHALL execute custom logic
3. IF custom validator throws error THEN system SHALL handle gracefully
```

#### Generated Tasks
```markdown
- [ ] 1. Create core validation framework
  - [ ] 1.1 Implement Validator base class
    - Create chainable validation methods
    - Add error collection and reporting
    - _Requirements: 1.1, 1.2_
  
  - [ ] 1.2 Build built-in validation rules
    - Add common validators (required, email, length, etc.)
    - Implement type checking validators
    - _Requirements: 1.1_

- [ ] 2. Add custom validation support
  - Create plugin system for custom validators
  - Add validator registration and lookup
  - Write comprehensive test suite
  - _Requirements: 2.1, 2.2, 2.3_
```

## DevOps/Infrastructure Examples

### Example 6: Container Health Monitoring

**Scenario:** Monitoring system for containerized applications.

#### Generated Requirements
```markdown
### Requirement 1
**User Story:** As a DevOps engineer, I want to monitor container health, so that I can detect and respond to issues quickly

#### Acceptance Criteria
1. WHEN container health check fails THEN system SHALL trigger alert
2. WHEN container is unhealthy for threshold period THEN system SHALL attempt restart
3. WHEN restart fails THEN system SHALL escalate to on-call team

### Requirement 2
**User Story:** As a developer, I want to see application metrics, so that I can understand performance characteristics

#### Acceptance Criteria
1. WHEN application starts THEN system SHALL begin collecting metrics
2. WHEN metrics exceed thresholds THEN system SHALL generate warnings
3. WHEN viewing dashboard THEN system SHALL display real-time metrics
```

#### Generated Design
```markdown
#### HealthMonitor Component
Monitors container and application health status

**Key Responsibilities:**
- Health check execution
- Metric collection and aggregation
- Alert generation and routing
- Dashboard data provision

### Technology Stack
- **Monitoring:** Prometheus + Grafana
- **Alerting:** AlertManager with PagerDuty integration
- **Container Runtime:** Docker with health check support
- **Service Discovery:** Consul for dynamic service registration
```

## Data Processing Examples

### Example 7: ETL Pipeline

**Scenario:** Data extraction, transformation, and loading pipeline.

#### Generated Requirements
```markdown
### Requirement 1
**User Story:** As a data analyst, I want to process large datasets reliably, so that I can generate accurate reports

#### Acceptance Criteria
1. WHEN processing fails THEN system SHALL retry with exponential backoff
2. WHEN data is corrupted THEN system SHALL log error and continue with valid records
3. WHEN processing completes THEN system SHALL generate processing summary

### Requirement 2
**User Story:** As a data engineer, I want to monitor pipeline performance, so that I can optimize processing efficiency

#### Acceptance Criteria
1. WHEN pipeline runs THEN system SHALL track processing time and throughput
2. WHEN performance degrades THEN system SHALL alert operations team
3. WHEN viewing metrics THEN system SHALL show historical performance trends
```

#### Generated Tasks
```markdown
- [ ] 1. Build data extraction framework
  - [ ] 1.1 Create configurable data source connectors
    - Implement database, API, and file connectors
    - Add connection pooling and retry logic
    - _Requirements: 1.1, 1.2_
  
  - [ ] 1.2 Add data validation and cleansing
    - Create data quality checks
    - Implement error handling and logging
    - _Requirements: 1.2_

- [ ] 2. Implement transformation engine
  - Create pluggable transformation system
  - Add parallel processing capabilities
  - Write performance monitoring hooks
  - _Requirements: 2.1, 2.2_
```

## Advanced Workflow Examples

### Example 8: Multi-Phase Feature Development

**Scenario:** Large feature requiring multiple development phases.

#### Phase 1: Core Infrastructure
```
M-x protagentic-create-spec RET
Spec name: messaging-system-core RET
```

#### Phase 2: User Interface
```
M-x protagentic-create-spec RET
Spec name: messaging-system-ui RET
```

#### Phase 3: Advanced Features
```
M-x protagentic-create-spec RET
Spec name: messaging-system-advanced RET
```

Each phase builds on the previous, with clear dependencies and integration points defined in the design documents.

### Example 9: Refinement Workflow

**Scenario:** Iteratively improving requirements based on stakeholder feedback.

#### Initial Requirements Review
```
M-x protagentic-refine-requirements RET

Quality Score: 45/100

Improvement Suggestions:
• Add user stories in 'As a [role], I want [feature], so that [benefit]' format
• Add acceptance criteria using WHEN/THEN or IF/THEN format
• Consider adding more requirements to cover edge cases

Choose action: (3) Add criteria

Add criteria to which requirement? Requirement 1
New acceptance criterion: WHEN user uploads file larger than 10MB THEN system SHALL compress before storage
```

#### Design Enhancement
```
M-x protagentic-refine-design RET

Quality Score: 72/100

Choose: (1) Components (2) Data models (3) Tech stack (4) Errors (5) Testing (6) Manual edit (7) Finalize: 2

Data model name: FileUpload
Model purpose: Tracks file upload metadata and processing status
Key fields: filename, size, mime_type, status, compression_ratio
```

## Tips for Effective Usage

### 1. Start Simple
Begin with basic requirements and gradually add complexity through refinement.

### 2. Use Descriptive Names
Choose spec names that clearly indicate the feature scope:
- ✅ `user-authentication`
- ✅ `payment-processing`
- ❌ `feature1`
- ❌ `new-stuff`

### 3. Leverage Refinement
Use the interactive refinement tools to improve document quality:
- Add missing acceptance criteria
- Enhance component descriptions
- Specify technology choices

### 4. Review Generated Content
Always review and customize generated templates to match your project needs.

### 5. Link Requirements to Tasks
Ensure implementation tasks clearly reference specific requirements for traceability.

## Common Patterns

### API Endpoint Development
1. **Requirements**: Define API contract and behavior
2. **Design**: Specify request/response formats, validation, error handling
3. **Tasks**: Implementation steps including tests, documentation, deployment

### UI Component Development
1. **Requirements**: User interactions and visual requirements
2. **Design**: Component architecture, state management, styling approach
3. **Tasks**: Component implementation, styling, integration, testing

### Data Migration Projects
1. **Requirements**: Data transformation rules and validation criteria
2. **Design**: Migration strategy, rollback procedures, performance considerations
3. **Tasks**: Script development, testing, monitoring, execution procedures

These examples demonstrate how Protagentic adapts to different project types while maintaining consistent structure and traceability throughout the development process.