# LLM Integration Setup Guide

This guide provides detailed instructions for setting up and configuring LLM integration in Protagentic.

## Quick Start

### 1. Get OpenAI API Key

1. Visit [OpenAI API Keys](https://platform.openai.com/api-keys)
2. Sign in or create an account
3. Click "Create new secret key"
4. Copy the key (starts with `sk-`)

### 2. Configure Protagentic

**Option A: Interactive Setup (Recommended)**
```
M-x protagentic-commands-setup-llm
```
This will guide you through choosing a storage method:
- **Environment Variable**: Sets `OPENAI_API_KEY` (recommended)
- **AuthInfo File**: Stores in `~/.authinfo` (secure)
- **Custom Variable**: Stores in Emacs config (less secure)

**Option B: Environment Variable (Manual)**
```bash
# Add to your shell profile (.bashrc, .zshrc, etc.)
export OPENAI_API_KEY="sk-your-api-key-here"

# Restart Emacs or reload configuration
```

**Option C: AuthInfo File (Manual)**
```bash
# Add to ~/.authinfo file
echo "machine api.openai.com login protagentic password sk-your-api-key-here port https" >> ~/.authinfo
chmod 600 ~/.authinfo
```

**Option D: Emacs Configuration (Not Recommended)**
```elisp
;; Add to your init.el (stores key in plain text)
(setq protagentic-llm-api-key "sk-your-api-key-here")
```

### 3. Test Configuration

```
M-x protagentic-config-validate-api-key
```

## Configuration Options

### Generation Modes

```elisp
;; Template mode - Fast, offline, no costs
(setq protagentic-config-default-generation-mode 'template)

;; LLM mode - High quality, requires API key
(setq protagentic-config-default-generation-mode 'llm)

;; Hybrid mode - LLM with template fallback (recommended)
(setq protagentic-config-default-generation-mode 'hybrid)

;; Always prompt for mode selection
(setq protagentic-config-prompt-for-mode t)
```

### Model Selection

```elisp
;; GPT-4 - Highest quality, more expensive
(setq protagentic-config-llm-model "gpt-4")

;; GPT-4 Turbo - Good balance of quality and cost
(setq protagentic-config-llm-model "gpt-4-turbo")

;; GPT-3.5 Turbo - Fastest, most economical
(setq protagentic-config-llm-model "gpt-3.5-turbo")
```

### Cost Management

```elisp
;; Set monthly spending limit (USD)
(setq protagentic-config-monthly-cost-limit 50.0)

;; Warn when reaching 80% of limit
(setq protagentic-config-cost-warning-threshold 0.8)

;; Enable usage tracking
(setq protagentic-config-enable-usage-tracking t)

;; Disable cost limiting (set to 0)
(setq protagentic-config-monthly-cost-limit 0)
```

### Response Quality

```elisp
;; Maximum tokens per response
(setq protagentic-config-llm-max-tokens 4000)  ; Default
(setq protagentic-config-llm-max-tokens 2000)  ; More economical
(setq protagentic-config-llm-max-tokens 6000)  ; More detailed

;; Temperature (creativity) - 0.0 to 2.0
(setq protagentic-config-llm-temperature 0.7)  ; Default (balanced)
(setq protagentic-config-llm-temperature 0.3)  ; More focused/consistent
(setq protagentic-config-llm-temperature 1.0)  ; More creative/varied
```

## Usage Examples

### Basic Workflow with LLM

```
1. M-x protagentic-create-spec
   Spec name: user-dashboard
   Generation mode: llm
   Feature description: A personalized user dashboard showing activity, notifications, and quick actions

2. M-x protagentic-generate-design
   Generation mode: llm

3. M-x protagentic-generate-tasks
   Generation mode: hybrid
```

### Regenerating with Different Modes

```
;; Switch from template to LLM for better quality
M-x protagentic-regenerate-requirements
Generation mode: llm

;; Try hybrid mode for reliability
M-x protagentic-regenerate-design
Generation mode: hybrid
```

### Monitoring Usage

```
;; Check current usage and costs
M-x protagentic-show-config

;; Reset usage statistics
M-x protagentic-reset-usage-stats
```

## Advanced Configuration

### Custom Prompts (Advanced Users)

```elisp
;; Enable custom prompt templates
(setq protagentic-prompts-use-custom-templates t)

;; Example: Company-specific requirements template
(defun my-company-requirements-prompt (context)
  "Custom requirements prompt following company standards."
  (concat
   "Generate requirements following our company methodology:\n"
   "- Use BDD format for acceptance criteria\n"
   "- Include security considerations\n"
   "- Reference compliance requirements\n\n"
   (protagentic-prompts-generate-requirements-prompt context)))

;; Override built-in prompt
(setf (protagentic-prompt-template-user-template 
       protagentic-prompts--requirements-template)
      'my-company-requirements-prompt)
```

### Project-Specific Configuration

```elisp
;; In .dir-locals.el for project-specific settings
((nil . ((protagentic-config-default-generation-mode . llm)
         (protagentic-config-llm-model . "gpt-4")
         (protagentic-config-llm-temperature . 0.5))))
```

### Integration with External Tools

```elisp
;; Hook into workflow completion
(add-hook 'protagentic-hook-functions
          (lambda (spec phase)
            (when (eq phase 'tasks)
              ;; Auto-create GitHub issues from tasks
              (my-create-github-issues spec))))

;; Custom cost tracking
(defun my-track-llm-costs (tokens cost)
  "Custom cost tracking function."
  (append-to-file 
   (format "%s: %d tokens, $%.4f\n" 
           (format-time-string "%Y-%m-%d %H:%M")
           tokens cost)
   nil "~/.protagentic-costs.log"))

(advice-add 'protagentic-config-track-usage :after #'my-track-llm-costs)
```

## Cost Optimization

### Model Comparison

| Model | Quality | Speed | Cost (per 1K tokens) | Best For |
|-------|---------|-------|---------------------|----------|
| GPT-4 | Highest | Slow | ~$0.03 | Complex features, critical specs |
| GPT-4 Turbo | High | Medium | ~$0.01 | Most use cases |
| GPT-3.5 Turbo | Good | Fast | ~$0.002 | Simple features, rapid prototyping |

### Cost-Saving Tips

1. **Use Hybrid Mode**: Automatic fallback prevents failed requests
2. **Optimize Token Limits**: Start with 2000 tokens, increase if needed
3. **Choose Right Model**: Use GPT-3.5 for simple features
4. **Batch Operations**: Generate all phases in one session
5. **Monitor Usage**: Check costs regularly with `protagentic-show-config`

### Example Cost Scenarios

**Light Usage (5 specs/month):**
- Model: GPT-3.5 Turbo
- Tokens per spec: ~1500
- Monthly cost: ~$0.15

**Medium Usage (20 specs/month):**
- Model: GPT-4 Turbo
- Tokens per spec: ~3000
- Monthly cost: ~$6.00

**Heavy Usage (50 specs/month):**
- Model: GPT-4
- Tokens per spec: ~4000
- Monthly cost: ~$60.00

## Troubleshooting

### Common Error Messages

**"Symbol's function definition is void: auth-source-store-secrets"**
```elisp
;; This error has been fixed. The package now uses multiple storage methods:
;; 1. Environment variables (recommended)
;; 2. ~/.authinfo file  
;; 3. Emacs custom variables
;; No longer depends on auth-source-store-secrets function
```

**"Symbol's function definition is void: customize-save-all"**
```elisp
;; This error has been fixed. The package now uses compatible functions:
;; - Tries customize-save-all first (newer Emacs)
;; - Falls back to custom-save-all (older Emacs)
;; - Graceful handling when neither is available
```

**"OpenAI API key not configured"**
```elisp
;; Solution: Set up API key
M-x protagentic-commands-setup-llm
;; Or set environment variable
```

**"Request timeout or connection failed"**
```elisp
;; Solution: Check network and increase timeout
(setq protagentic-llm-request-timeout 120)  ; 2 minutes
```

**"Monthly cost limit exceeded"**
```elisp
;; Solution: Increase limit or reset usage
(setq protagentic-config-monthly-cost-limit 100.0)
;; Or reset stats
M-x protagentic-reset-usage-stats
```

**"Invalid API key"**
- Verify key is correct and active
- Check OpenAI account has credits
- Ensure key has proper permissions

### Debug Mode

```elisp
;; Enable detailed error messages
(setq debug-on-error t)

;; Check LLM availability
(protagentic-llm-available-p)

;; Test API connection
(protagentic-llm-validate-credentials)

;; Check configuration
M-x protagentic-show-config
```

### Performance Issues

**Slow Generation:**
- Use GPT-3.5 Turbo for faster responses
- Reduce max tokens: `(setq protagentic-config-llm-max-tokens 2000)`
- Check network connection

**Poor Quality Output:**
- Increase temperature for more creativity
- Use GPT-4 for better reasoning
- Provide more detailed feature descriptions

## Security Considerations

### API Key Security

1. **Never commit API keys to version control**
2. **Use environment variables or secure storage**
3. **Rotate keys regularly**
4. **Monitor usage for unauthorized access**

### Data Privacy

- Feature descriptions are sent to OpenAI
- Generated content may be used for model training
- Consider using Azure OpenAI for enterprise compliance
- Review OpenAI's data usage policies

### Best Practices

```elisp
;; Use environment variables
(setenv "OPENAI_API_KEY" "sk-your-key")

;; Or secure Emacs storage (encrypted)
M-x protagentic-commands-setup-llm  ; Uses auth-source

;; Never do this in shared configs:
;; (setq protagentic-llm-api-key "sk-key")  ; DON'T DO THIS
```

## Compatibility

### Emacs Version Support
- **Emacs 26.1+**: Full support with all features
- **Emacs 25.x**: Basic support (some advanced features may be limited)
- **Emacs 24.x**: Limited support (manual configuration may be required)

### Function Compatibility
The package automatically detects available functions and provides fallbacks:
- `auth-source-store-secrets` → Multiple storage methods
- `customize-save-all` → `custom-save-all` fallback
- `project.el` → `projectile` → manual fallback

### Platform Support
- **macOS**: Full support
- **Linux**: Full support  
- **Windows**: Basic support (some file permission features limited)

## Support and Resources

- [OpenAI API Documentation](https://platform.openai.com/docs)
- [OpenAI Pricing](https://openai.com/pricing)
- [Protagentic GitHub Issues](https://github.com/kgthegreat/protagentic/issues)
- [Emacs auth-source Documentation](https://www.gnu.org/software/emacs/manual/html_node/auth/index.html)

## Example Configurations

### Minimal Setup (Cost-Conscious)
```elisp
(setq protagentic-config-default-generation-mode 'hybrid)
(setq protagentic-config-llm-model "gpt-3.5-turbo")
(setq protagentic-config-llm-max-tokens 2000)
(setq protagentic-config-monthly-cost-limit 10.0)
(setq protagentic-config-prompt-for-mode t)
```

### Premium Setup (Quality-Focused)
```elisp
(setq protagentic-config-default-generation-mode 'llm)
(setq protagentic-config-llm-model "gpt-4")
(setq protagentic-config-llm-max-tokens 6000)
(setq protagentic-config-llm-temperature 0.8)
(setq protagentic-config-monthly-cost-limit 100.0)
```

### Enterprise Setup (Balanced)
```elisp
(setq protagentic-config-default-generation-mode 'hybrid)
(setq protagentic-config-llm-model "gpt-4-turbo")
(setq protagentic-config-llm-max-tokens 4000)
(setq protagentic-config-monthly-cost-limit 50.0)
(setq protagentic-config-cost-warning-threshold 0.8)
(setq protagentic-config-enable-usage-tracking t)
```