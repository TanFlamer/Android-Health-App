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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        holder.nameView.setText(sportRecyclerItem.getName());
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

        Map<LinearLayout, Boolean> linearLayoutBooleanMap = new HashMap<>();
        TextView titleView, nameView, totalTimeView, totalCalorieView, averageTimeView, totalDaysView, averageCalorieView, longestTimeView, shortestTimeView, mostCalorieView, leastCalorieView;
        LinearLayout layoutVisible, layoutHidden;

        public SportRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sportTitle);
            nameView = itemView.findViewById(R.id.sportName);
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

            hideLayout(itemView.findViewById(R.id.sportNameVisible), itemView.findViewById(R.id.sportNameHidden));
            hideLayout(itemView.findViewById(R.id.sportTotalTimeVisible), itemView.findViewById(R.id.sportTotalTimeHidden));
            hideLayout(itemView.findViewById(R.id.sportTotalCalorieVisible), itemView.findViewById(R.id.sportTotalCalorieHidden));
            hideLayout(itemView.findViewById(R.id.sportDaysVisible), itemView.findViewById(R.id.sportDaysHidden));
            hideLayout(itemView.findViewById(R.id.sportAverageTimeVisible), itemView.findViewById(R.id.sportAverageTimeHidden));
            hideLayout(itemView.findViewById(R.id.sportAverageCalorieVisible), itemView.findViewById(R.id.sportAverageCalorieHidden));
            hideLayout(itemView.findViewById(R.id.sportLongestVisible), itemView.findViewById(R.id.sportLongestHidden));
            hideLayout(itemView.findViewById(R.id.sportShortestVisible), itemView.findViewById(R.id.sportShortestHidden));
            hideLayout(itemView.findViewById(R.id.sportMostVisible), itemView.findViewById(R.id.sportMostHidden));
            hideLayout(itemView.findViewById(R.id.sportLeastVisible), itemView.findViewById(R.id.sportLeastHidden));
        }

        public void hideLayout(LinearLayout layoutVisible, LinearLayout layoutHidden){
            layoutHidden.setVisibility(View.GONE);
            linearLayoutBooleanMap.put(layoutHidden, false);
            layoutVisible.setOnClickListener(view -> {
                linearLayoutBooleanMap.put(layoutHidden, Boolean.FALSE.equals(linearLayoutBooleanMap.get(layoutHidden)));
                layoutHidden.setVisibility(Boolean.TRUE.equals(linearLayoutBooleanMap.get(layoutHidden)) ? View.VISIBLE : View.GONE);
            });
        }
    }
}
