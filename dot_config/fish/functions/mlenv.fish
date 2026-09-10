function mlenv --description "Bootstrap a new ML/AI project with uv"
    set -l project_name $argv[1]
    if test -z "$project_name"
        echo "Usage: mlenv <project-name> [type]"
        echo "Types: dl (deep learning), ml (classical ML), llm (LLM/GenAI)"
        return 1
    end

    set -l project_type $argv[2]
    test -z "$project_type"; and set project_type "ml"

    uv init $project_name
    cd $project_name

    # Common scientific stack
    uv add numpy scipy matplotlib seaborn ipykernel

    switch $project_type
        case dl
            uv add torch torchvision torchaudio "jax[cpu]"
            uv add wandb tensorboard
        case ml
            uv add scikit-learn pandas polars xgboost
            uv add wandb mlflow
        case llm
            uv add transformers tokenizers datasets accelerate
            uv add langchain
            uv add wandb
    end

    # Register Jupyter kernel
    uv run python -m ipykernel install --user --name $project_name

    echo "ML project '$project_name' ($project_type) ready!"
end
