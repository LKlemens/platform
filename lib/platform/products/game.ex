defmodule Platform.Products.Game do
  use Ecto.Schema
  alias Platform.Accounts.Player
  alias Platform.Products.Gameplay
  import Ecto.Changeset

  schema "games" do
    many_to_many(:players, Player, join_through: Gameplay)

    field(:description, :string)
    field(:featured, :boolean, default: false)
    field(:slug, :string, unique: true)
    field(:thumbnail, :string)
    field(:title, :string)

    timestamps()
  end

  @doc false
  def changeset(game, attrs) do
    game
    |> cast(attrs, [:description, :featured, :slug, :thumbnail, :title])
    |> validate_required([:description, :featured, :slug, :thumbnail, :title])
    |> unique_constraint(:slug)
  end
end
