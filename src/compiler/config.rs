use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LycheeConfig {
    pub package: Package,
    pub dependencies: Option<HashMap<String, Dependency>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct ConfigData {
    pub config: LycheeConfig,
    pub root_path: PathBuf,
}

impl ConfigData {
    pub fn get_entry_point(&self) -> ConfigResult<PathBuf> {
        let path = self.root_path.join("src/main.lyc");
        if !path.try_exists()? {
            return Err(anyhow::anyhow!("Entry point not found at {:?}", path));
        }
        Ok(path)
    }
}

pub fn read_config(root_path: &PathBuf) -> ConfigResult<ConfigData> {
    let config_path = root_path.join("lychee.toml");
    if !config_path.try_exists()? {
        return Err(anyhow::anyhow!(
            "Config file not found at {:?}",
            config_path
        ));
    }
    let config_str = std::fs::read_to_string(config_path.clone())
        .unwrap_or_else(|_| panic!("Failed to read config file at {config_path:?}"));
    match toml::from_str(&config_str) {
        Ok(config) => Ok(ConfigData {
            config,
            root_path: root_path.clone(),
        }),
        Err(e) => {
            panic!("Failed to parse config file at {:?}: {}", root_path, e);
        }
    }
}

pub type ConfigResult<T> = Result<T, anyhow::Error>;

pub fn find_all_entry_points(config: &ConfigData) -> ConfigResult<Vec<ConfigData>> {
    let mut visited = HashSet::new();
    let mut entries = vec![config.clone()];
    find_all_entry_points_inner(config, &mut visited, &mut entries)?;
    Ok(entries)
}

fn find_all_entry_points_inner(
    config: &ConfigData,
    visited: &mut HashSet<String>,
    entries: &mut Vec<ConfigData>,
) -> ConfigResult<()> {
    if visited.contains(&config.config.package.name) {
        return Err(anyhow::anyhow!(
            "Circular dependency to root '{}'",
            config.config.package.name
        ));
    }
    visited.insert(config.config.package.name.clone());

    if let Some(deps) = &config.config.dependencies {
        for (dep_name, dep) in deps {
            let config_path = config.root_path.join(&dep.path);
            let dep_config = read_config(&config_path)?;
            if dep_config.config.package.name != *dep_name {
                return Err(anyhow::anyhow!(
                    "Dependency name mismatch: expected '{}', found '{}'",
                    dep_name,
                    dep_config.config.package.name
                ));
            }
            find_all_entry_points_inner(&dep_config, visited, entries)?;
            entries.push(dep_config);
        }
    }

    Ok(())
}
