# mix.exs
defmodule KhepriStore.MixProject do
  use Mix.Project

  def project do
    [
      app: :khepri_store,
      version: "0.1.0",
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:khepri, "~> 0.8.0"}
    ]
  end
end
