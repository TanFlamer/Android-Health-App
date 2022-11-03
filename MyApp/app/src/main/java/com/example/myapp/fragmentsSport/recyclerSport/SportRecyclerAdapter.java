package com.example.myapp.fragmentsSport.recyclerSport;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;

import java.util.List;

public class SportRecyclerAdapter extends RecyclerView.Adapter<SportRecyclerAdapter.SportRecyclerItemViewHolder> {

    Context context;
    List<SportRecyclerItem> sportRecyclerItemList;

    public SportRecyclerAdapter(Context context, List<SportRecyclerItem> sportRecyclerItemList){
        this.context = context;
        this.sportRecyclerItemList = sportRecyclerItemList;
    }

    @NonNull
    @Override
    public SportRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sport_recycler_list_item, parent, false);
        return new SportRecyclerItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SportRecyclerItemViewHolder holder, int position) {
        SportRecyclerItem sportRecyclerItem = sportRecyclerItemList.get(position);

        holder.titleView.setText(sportRecyclerItem.getTitle());
        holder.totalTimeView.setText(String.valueOf(sportRecyclerItem.getTotalTime()));
        holder.totalCalorieView.setText(String.valueOf(sportRecyclerItem.getTotalCalorie()));
        holder.totalDaysView.setText(String.valueOf(sportRecyclerItem.getTotalDays()));
        holder.averageTimeView.setText(String.valueOf(sportRecyclerItem.getAverageTime()));
        holder.averageCalorieView.setText(String.valueOf(sportRecyclerItem.getAverageCalorie()));
        holder.longestTimeView.setText(String.valueOf(sportRecyclerItem.getLongestTime()));
        holder.shortestTimeView.setText(String.valueOf(sportRecyclerItem.getShortestTime()));
        holder.mostCalorieView.setText(String.valueOf(sportRecyclerItem.getMostCalorie()));
        holder.leastCalorieView.setText(String.valueOf(sportRecyclerItem.getLeastCalorie()));

        boolean isShown = sportRecyclerItemList.get(position).isShown();
        holder.layoutHidden.setVisibility(isShown ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sportRecyclerItemList.size();
    }

    public class SportRecyclerItemViewHolder extends RecyclerView.ViewHolder{

        TextView titleView, totalTimeView, totalCalorieView, averageTimeView, totalDaysView, averageCalorieView, longestTimeView, shortestTimeView, mostCalorieView, leastCalorieView;
        LinearLayout layoutVisible, layoutHidden;

        public SportRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sportTitle);
            totalTimeView = itemView.findViewById(R.id.sportTotalTime);
            totalCalorieView = itemView.findViewById(R.id.sportTotalCalorie);
            totalDaysView = itemView.findViewById(R.id.sportTotalDays);
            averageTimeView = itemView.findViewById(R.id.sportAverageTime);
            averageCalorieView = itemView.findViewById(R.id.sportAverageCalorie);
            longestTimeView = itemView.findViewById(R.id.sportLongestTime);
            shortestTimeView = itemView.findViewById(R.id.sportShortestTime);
            mostCalorieView = itemView.findViewById(R.id.sportMostCalorie);
            leastCalorieView = itemView.findViewById(R.id.sportLeastCalorie);

            layoutVisible = itemView.findViewById(R.id.sportLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sportLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                SportRecyclerItem sportRecyclerItem = sportRecyclerItemList.get(getAdapterPosition());
                sportRecyclerItem.setShown(!sportRecyclerItem.isShown());
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
